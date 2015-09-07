{-# LANGUAGE MagicHash, DeriveDataTypeable, UnliftedFFITypes #-}

-- |
-- Stability   : experimental
-- Portability : GHC
--
-- This module is intended to be imported @qualified@ in order to
-- avoid name clashes with the "Prelude", "Data.ByteString", and
-- "Data.ByteString.Lazy" modules. E.g.:
--
-- > import qualified Data.ByteString.Plain as PB
--
module Data.ByteString.Plain (
    -- * The plain @ByteString@ type and representation
      ByteString
    -- * Introducing and eliminating 'ByteString's
    , empty
    , fromStrict
    , toStrict
    -- * Basic operations
    , null
    , length
    ) where

import           Control.DeepSeq (NFData(rnf))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Typeable            (Typeable)
import           GHC.ForeignPtr
import           GHC.Prim
import           GHC.Types
import           Prelude hiding (length, null)
import           System.IO.Unsafe (unsafePerformIO)

-- |Compact heap representation a (strict) 'B.ByteString' can be (un)wrapped to/from.
--
-- This data type depends on the ordinary 'B.ByteString' type to be
-- useful but comes with a different cost-model.
--
-- This representation avoids the 'ForeignPtr' indirection, and the
-- offset/length slice representation for shared 'B.ByteString's, and
-- is therefore suitable if you need to store many small strings in a
-- data records or for use as keys in container types. On the other
-- hand, string operations on 'ByteString' would require
-- re-allocations, and thus are not supported. If you need to perform
-- such operations convert and operate on conventional 'B.ByteString's
-- instead.
--
-- This structure supports @UNPACK@, and then has an overhead
-- of only 3 words (beyond the word-padded storage of the byte-string
-- payload), as it's basically just a pointer to a
-- 'MutableByteArray#':
--
-- > data ByteString = PBS !(MutableByteArray# RealWorld)
--
-- In contrast, a single non-shared unpacked ('PlainPtr'-backed)
-- 'ByteString' field exhibits an overhead of 8 words:
--
-- > data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload (2 words)
-- >                      {-# UNPACK #-} !Int                -- offset (1 word)
-- >                      {-# UNPACK #-} !Int                -- length (1 word)
-- >
-- > data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents -- 2 words w/o info-ptr
-- >
-- > data ForeignPtrContents -- 1 word needed for info-ptr
-- >     = PlainForeignPtr {...}
-- >     | MallocPtr {...}
-- >     | PlainPtr (MutableByteArray# RealWorld)  -- common case (1 word)
-- >
-- > data MutableByteArray# s -- 2 words + payload
--
-- As an optimization, all zero-length strings are mapped to the
-- singleton 'empty' value.
data ByteString = PBS !(MutableByteArray# RealWorld)
                  deriving Typeable

-- |Singleton value the 'B.empty' 'B.ByteString' is mapped to/from.
empty :: ByteString
empty = unsafePerformIO $ do
    (ForeignPtr _ (PlainPtr mbarr#)) <- mallocPlainForeignPtrBytes 0
    return $! PBS mbarr#
{-# NOINLINE empty #-}

-- |Extract 'ByteString' from 'ByteString'
--
-- If possible, the internally used 'MutableByteArray#' is shared with
-- the original 'ByteString' in which case the conversion is cheap.
--
-- However, if necessary, a trimmed copy of the original 'ByteString'
-- will be created via 'B.copy' resulting in a newly allocated
-- 'MutableByteArray#'.
--
-- N.B.: Because strict 'B.ByteString's use pinned memory internally
-- also plain 'ByteString's use pinned memory and thereby increase the
-- potential for memory fragmentation as the garbage collector is not
-- allowed to move pinned memory areas.
--
-- Depending on the use case, it might be beneficial to apply some
-- form of memoizing to the 'fromStrict' conversion (also known as
-- <http://en.wikipedia.org/wiki/Hash_consing Hash consing> or
-- <http://en.wikipedia.org/wiki/String_interning String interning>).

fromStrict :: B.ByteString -> ByteString
fromStrict (B.PS _ _ 0) = empty
fromStrict (B.PS (ForeignPtr _ (PlainPtr mbarr#)) 0 l)
  | l' == l            = PBS mbarr#
  where
    l' = I# (sizeofMutableByteArray# mbarr#)
fromStrict bs = fromStrict (B.copy bs) -- this assumes
                    -- ByteString internals return a trimmed
                    -- ForeignPtr/PlainPtr string; otherwise there's
                    -- risk of infinite looping
{-# INLINE fromStrict #-}

-- |Convert a plain 'ByteString' back into a 'ByteString'.
--
-- This effectively wraps the plain 'ByteString' into a 'ForeignPtr'
-- and a plain 'B.ByteString' type.
toStrict :: ByteString -> B.ByteString
toStrict (PBS mbarr#) | l == 0     = B.empty
                      | otherwise  = B.PS fp 0 l
  where
    l  = I# (sizeofMutableByteArray# mbarr#)
    addr = byteArrayContents# (unsafeCoerce# mbarr#)
    fp = ForeignPtr addr (PlainPtr mbarr#)
{-# INLINE toStrict #-}

null :: ByteString -> Bool
null = (==0) . length
{-# INLINE null #-}

length :: ByteString -> Int
length (PBS mbarr#) = I# (sizeofMutableByteArray# mbarr#)
{-# INLINE length #-}

-- WHNF == NF
instance NFData ByteString where rnf x = seq x ()

-- the following instances are implement via strict 'B.ByteString's;
-- In the future "native" implementations shall be provided if they
-- result being faster.
instance Eq ByteString where
    x == y  = toStrict x == toStrict y
    {-# INLINE (==) #-}

instance Ord ByteString where
    compare x y  = compare (toStrict x) (toStrict y)
    {-# INLINE compare #-}

instance Hashable ByteString where
    hashWithSalt salt = hashWithSalt salt . toStrict
    {-# INLINE hashWithSalt #-}

-- Mostly for convenience
instance Show ByteString where
    showsPrec p rbs = showsPrec p (toStrict rbs)

instance Read ByteString where
    readsPrec p str = [ (fromStrict x, y) | (x, y) <- readsPrec p str ]
