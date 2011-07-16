--Haskellでpingを送る
--バイナリ処理・生ソケットを触る練習
--------------------------------------------------------------------------------
module Main where

import Foreign
import Control.Monad
import Network.Socket
import System.Environment
import Network.BSD
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data ICMPHeader = EchoMessage {
    ecType      ::  Word8,
    ecCode      ::  Word8,
    ecChkSum    ::  Word16,
    ecIdentifier::  Word16,
    ecSeqNum    ::  Word16
}

instance Binary ICMPHeader where
    put h = do
        putWord8 $ ecType h
        putWord8 $ ecCode h
        putWord16be $ ecChkSum h
        putWord16be $ ecIdentifier h
        putWord16be $ ecSeqNum h

    get = do
        typ <- getWord8
        code <- getWord8
        chk <- getWord16be
        ident <- getWord16be
        seq <- getWord16be
        return $ EchoMessage {
                    ecType = typ,
                    ecCode = code,
                    ecChkSum = chk,
                    ecIdentifier = ident,
                    ecSeqNum = seq
                }

--IPヘッダでのICMPのプロトコル番号
kIPPROTO_ICMP            =   1
--IPヘッダでのプロトコル番号のオフセット
kIPProtocolTypeOffset    =   9
--IPヘッダの長さ
kIPHeaderLength          =   20
--ICMPヘッダでのチェックサムのオフセット
kICMPChkSumOffset        =   2
--ICMPヘッダの長さ
kICMPHeaderLength        =   8
--ICMPパケットの通知の種類
--ICMP_ECHOを送れば、ICMP_ECHOREPLYが返ってくるはず
kICMP_ECHOREPLY          =   0
kICMP_ECHO               =   8

--異なるデータ長のデータ型を1つの代数的データ型にラップする
data StructMember = W8 Word8
    |W16    Word16
    |W32    Word32

--与えられた長さのバッファのチェックサムを計算する
--1の補数和の補数
calcChkSum :: Ptr a -> Int -> IO Word16
calcChkSum ptr size = do
    values <- mapM (peekByteOff ptr) [0, 2 .. size - 2] :: IO [Word16]
    rest <- peekByteOff ptr $ size - 1 :: IO Word8
    let values' = if odd size
                    then (fromIntegral rest) : values
                    else values
    return $ calcChkSum' values'
    where
        calcChkSum' values =
            fromIntegral
                . complement . carry . carry . sum . map fromIntegral $ values
        carry :: Word32 -> Word32
        carry x = (x .&. 0xffff) + (x `shiftR` 16)

--アラインメントの整ったStructMemberのリストを受け取り、バッファに書き込む
writeAlignedStruct :: Ptr a -> [StructMember] -> IO ()
writeAlignedStruct ptr members =
    forM_ (zip members offsets) writeStructMember
    where
        sizeOfMember member =
            case member of
                W8  v   ->  sizeOf v
                W16 v   ->  sizeOf v
                W32 v   ->  sizeOf v
        offsets =
            take (length members) $ scanl (+) 0 $ map sizeOfMember members
        writeStructMember (member, offset) =
            case member of
                W8  v   ->  pokeByteOff ptr offset v
                W16 v   ->  pokeByteOff ptr offset v
                W32 v   ->  pokeByteOff ptr offset v

main = do
    --送受信のためのバッファを用意
    buf <- mallocBytes (kIPHeaderLength + kICMPHeaderLength)
    
    --ICMPヘッダを構築
    writeAlignedStruct buf [W8 kICMP_ECHO, W8 0, W16 0, W16 12345, W16 0]
    --ICMPヘッダのチェックサムを計算し、バッファに書き込む
    chksum <- calcChkSum buf kICMPHeaderLength
    pokeByteOff buf kICMPChkSumOffset chksum
    
    --ソケットを作成し、コマンドライン引数のホストのアドレスをルックアップ
    sock <- socket AF_INET Raw kIPPROTO_ICMP
    host <- getArgs
    hostentry <- getHostByName $ head host

    --ICMPパケットを送信、レスポンスを受信
    sendBufTo sock buf kICMPHeaderLength $ SockAddrInet 0 $ hostAddress hostentry
    recvBufFrom sock buf (kIPHeaderLength + kICMPHeaderLength)

    --IPヘッダ中のプロトコル番号と、ICMP通知の種類を取得
    protocol <- peekByteOff buf kIPProtocolTypeOffset :: IO Word8
    icmptype <- peekByteOff buf kIPHeaderLength :: IO Word8

    --プロトコルがICMPで、通知がECHOREPLYならping成功
    if protocol == fromIntegral kIPPROTO_ICMP && icmptype == kICMP_ECHOREPLY
        then putStrLn "Response came!"
        else putStrLn "Response error!"

    --ソケットを閉じる
    sClose sock
    --バッファを解放
    free buf
