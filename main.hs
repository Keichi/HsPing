--Haskellでpingを送る
--バイナリ処理・生ソケットを触る練習
--------------------------------------------------------------------------------
module Main where

import Data.Bits
import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment
import Network.BSD
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Time

--ICMPヘッダを表す型
data ICMPHeader = EchoMessage {
    ecType      ::  Word8,
    ecCode      ::  Word8,
    ecChkSum    ::  Word16,
    ecIdentifier::  Word16,
    ecSeqNum    ::  Word16
} deriving (Show, Eq)

--ICMPHeaderをバイナリにシリアライズ・デシリアライズできるように
instance Binary ICMPHeader where
    put h = do
        putWord8 $ ecType h
        putWord8 $ ecCode h
        putWord16le $ ecChkSum h
        putWord16be $ ecIdentifier h
        putWord16be $ ecSeqNum h
    get = do
        typ <- getWord8
        code <- getWord8
        chk <- getWord16le
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

--与えられた長さのバッファのチェックサムを計算する
--1の補数和の補数
calcChkSum src = do
    calcChkSum' $ packTo16 $ B.unpack $ pad src
    where
        --バッファの長さが奇数バイトなら末尾に0を足して偶数にする
        pad xs
            | odd $ B.length xs = xs `B.snoc` 0
            | otherwise         = xs

        --2バイトごとにまとめてWord16の配列にする
        packTo16 :: [Word8] -> [Word16]
        packTo16 [] = []
        packTo16 (x1:x2:xs) =
            (fromIntegral x2 `shiftL` 8 .|. fromIntegral x1):(packTo16 xs)
        
        --実際にチェックサムを計算する 
        calcChkSum' values =
            fromIntegral
                . complement . carry . carry . sum . map fromIntegral $ values
        carry :: Word32 -> Word32
        carry x = (x .&. 0xffff) + (x `shiftR` 16)

--チェックサムを付加する
addChkSum bs chk =
    encode $ (decode bs) {ecChkSum = chk}

--Data.ByteStringとData.ByteString.Lazyを相互変換する
deLazy = B.pack . BL.unpack
enLazy = BL.pack . B.unpack

main = do
    let buf = deLazy . encode $ EchoMessage {
                        ecType = kICMP_ECHO,
                        ecCode = 0,
                        ecSeqNum = 0,
                        ecIdentifier = 1234,
                        ecChkSum = 0
                        }
    let chksum = calcChkSum buf
    --ソケットを作成し、コマンドライン引数のホストのアドレスをルックアップ
    sock <- socket AF_INET Raw kIPPROTO_ICMP
    host <- getArgs
    hostentry <- getHostByName $ head host

    let buf2 = deLazy $ addChkSum (BL.pack $ B.unpack buf) chksum

    --ICMPパケットを送信、レスポンスを受信
    startTime <- getClockTime
    sendAllTo sock buf2 $ SockAddrInet 0 $ hostAddress hostentry
    (resp, addr) <- recvFrom sock (kIPHeaderLength + kICMPHeaderLength)

    --IPヘッダ中のプロトコル番号と、ICMP通知の種類を取得
    let protocolNum = resp `B.index` kIPProtocolTypeOffset
        respICMP = (decode . BL.drop (fromIntegral kIPHeaderLength) . enLazy $ resp :: ICMPHeader)
        respType = ecType respICMP
        respSeqNum = ecSeqNum respICMP

    if protocolNum == fromIntegral kIPPROTO_ICMP && respType == kICMP_ECHOREPLY
    then putStrLn "Echo reply received"
    else putStrLn "Something is wrong!"

    endTime <- getClockTime
    putStr "Elapsed Time:"
    putStr . show . (/10^9) .fromIntegral .  tdPicosec $ diffClockTimes endTime startTime
    putStrLn "[ms]"

    --ソケットを閉じる
    sClose sock
