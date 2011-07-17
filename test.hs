import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B

pad xs
    | odd $ B.length xs = xs `B.snoc` 0
    | otherwise         = xs

packTo16 :: [Word8] -> [Word16]
packTo16 [] = []
packTo16 (x1:x2:xs) =
    (fromIntegral x2 `shiftL` 8 .|. fromIntegral x1):(packTo16 xs)

calcChkSum :: B.ByteString -> Word16
calcChkSum src = do
    calcChkSum' $ packTo16 $ B.unpack $ pad src
    where
        calcChkSum' values =
            fromIntegral
                . complement . carry . carry . sum . map fromIntegral $ values
        carry :: Word32 -> Word32
        carry x = (x .&. 0xffff) + (x `shiftR` 16)

main = do
    --let buf = B.pack [1, 2, 3, 4, 5]
    print $ packTo16 $ B.unpack $ pad $ B.pack [1, 2, 3, 4, 5]
    --print $ calcChkSum buf
