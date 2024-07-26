{-# LINE 1 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}



module Data.Number.MPFR.FFIhelper where

import Data.Word

import Data.Int

import Foreign.C.String(CString)

{-# LINE 13 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
import Foreign.C.Types(CULong(..), CLong(..), CInt(..), CUInt(..), CDouble(..), CChar)

{-# LINE 17 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
import Foreign.Ptr(Ptr)
import Foreign.Marshal(alloca)
import Foreign.Storable
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrBytes)

import Data.Typeable(Typeable)
import GHC.Generics(Generic)

import Data.Function(on)

data RoundMode = Near | Zero | Up | Down | MPFR_RNDNA
                 deriving (Show, Read)

instance Enum RoundMode where

{-# LINE 44 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    fromEnum Near        = 0
{-# LINE 45 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    fromEnum Zero        = 1
{-# LINE 46 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    fromEnum Up          = 2
{-# LINE 47 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    fromEnum Down        = 3
{-# LINE 48 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    fromEnum MPFR_RNDNA   = -1
{-# LINE 49 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

    toEnum 0    = Near
{-# LINE 51 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    toEnum 1    = Zero
{-# LINE 52 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    toEnum 2    = Up
{-# LINE 53 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    toEnum 3    = Down
{-# LINE 54 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    toEnum (-1) = MPFR_RNDNA
{-# LINE 55 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

{-# LINE 56 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    toEnum i                    = error $ "RoundMode.toEnum called with illegal argument :" ++ show i


data MPFR = MP { precision :: {-# UNPACK #-} !CPrecision,
                 sign :: {-# UNPACK #-} !Sign,
                 exponent :: {-# UNPACK #-} !Exp,
                 limbs :: {-# UNPACK #-} !(ForeignPtr Limb)
} deriving (Typeable, Generic)

instance Storable MPFR where
    sizeOf _ = (32)
{-# LINE 67 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    alignment _ = alignment (undefined :: Int64)
{-# LINE 68 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
    peek = error "MPFR.peek: Not needed and not applicable"
    poke p (MP prec s e fp) = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) p prec
{-# LINE 70 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                                 (\hsc_ptr -> pokeByteOff hsc_ptr 8) p s
{-# LINE 71 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                                 (\hsc_ptr -> pokeByteOff hsc_ptr 16) p e
{-# LINE 72 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                                 withForeignPtr fp $ \p1 -> (\hsc_ptr -> pokeByteOff hsc_ptr 24) p p1
{-# LINE 73 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

newtype Precision = Precision { runPrec :: Word } deriving (Eq, Ord, Show, Enum)

instance Num Precision where
    (Precision w) + (Precision w') = Precision $ w + w'
    (Precision w) * (Precision w') = Precision $ w * w'
    (Precision a) - (Precision b) =
        if a >= b
        then Precision (a - b)
        else error $ "instance Precision Num (-): " ++
                       "Operation would result in negative precision."
    negate = error $ "instance Precision Num negate: " ++
                       "operation would result in negative precision"
    abs = id
    signum (Precision x) = Precision . signum $ x
    fromInteger i = if i >= 0
                    then Precision . fromInteger $ i
                    else error $ "instance Precision Num fromInteger: " ++
                             "operation would result  in negative precision"

instance Real Precision where
    toRational (Precision w) = toRational w

instance Integral Precision where
    quotRem (Precision w) (Precision w') = uncurry ((,) `on` Precision) $ quotRem w w'
    toInteger (Precision w) = toInteger w

{-# INLINE peekNoLimbPrec #-}
peekNoLimbPrec      :: Ptr MPFR -> IO (Sign, Exp)
peekNoLimbPrec p = do r21 <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 103 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      r22 <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 104 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      return (r21, r22)


{-# INLINE peekP #-}
peekP      :: Ptr MPFR -> ForeignPtr Limb -> IO MPFR
peekP p fp = do r11 <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 110 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                r21 <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 111 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                r22 <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 112 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                return (MP r11 r21 r22 fp)
{-# INLINE withDummy #-}
withDummy     :: Precision -> (Ptr MPFR -> IO CInt) -> IO (MPFR, Int)
withDummy w f =
    do alloca $ \ptr -> do
                      ls <- mpfr_custom_get_size (fromIntegral . runPrec $ w)
                      fp <- mallocForeignPtrBytes (fromIntegral ls)
                      (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (fromIntegral w :: CPrecision)
{-# LINE 120 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (1 :: Sign)
{-# LINE 121 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr (0 :: Exp)
{-# LINE 122 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      withForeignPtr fp $ \p1 -> (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr p1
{-# LINE 123 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                      r2 <- f ptr
                      r1 <- peekP ptr fp
                      return (r1, fromIntegral r2)

{-# INLINE pokeDummy #-}
pokeDummy          :: Ptr MPFR -> ForeignPtr Limb -> Precision -> IO ()
pokeDummy ptr fp p = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr ((fromIntegral . runPrec $ p) :: CPrecision)
{-# LINE 130 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (0 :: Sign)
{-# LINE 131 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr (0 :: Exp)
{-# LINE 132 "src/Data/Number/MPFR/FFIhelper.hsc" #-}
                        withForeignPtr fp $ \p1 -> (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr p1
{-# LINE 133 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

bitsPerMPLimb :: Int
bitsPerMPLimb = 8 * (8)
{-# LINE 136 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

bitsPerIntegerLimb :: Int
bitsPerIntegerLimb = bitsPerMPLimb

expZero :: Exp
expZero = -9223372036854775807
{-# LINE 142 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

expNaN :: Exp
expNaN = -9223372036854775806
{-# LINE 145 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

expInf :: Exp
expInf = -9223372036854775805
{-# LINE 148 "src/Data/Number/MPFR/FFIhelper.hsc" #-}


type CRoundMode = CInt

type Limb = Word64
{-# LINE 153 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

type Sign = Int32
{-# LINE 155 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

type CPrecision = Int64
{-# LINE 157 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

type Exp = Int64
{-# LINE 159 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

type MpSize = Int64
{-# LINE 161 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

-- utility functions from chsmpfr.h
foreign import ccall unsafe "initS"
        initS :: CPrecision -> IO (Ptr MPFR)

foreign import ccall unsafe "new_gmp_randstate"
        new_gmp_randstate :: IO (Ptr GmpRandState)

foreign import ccall unsafe "mpfr_urandomb_deref_randstate"
        mpfr_urandomb_deref_randstate ::  (Ptr MPFR) -> (Ptr GmpRandState) -> IO Int

type GmpRandState = ()

--------------------
foreign import ccall unsafe "mpfr_get_prec_wrap"
        mpfr_get_prec :: Ptr MPFR -> IO CPrecision

----------------------------------------------------------------

-- assignment functions
foreign import ccall unsafe "mpfr_set_wrap"
        mpfr_set :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_ui_wrap"
        mpfr_set_ui :: Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_si_wrap"
        mpfr_set_si :: Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_d"
        mpfr_set_d :: Ptr MPFR -> CDouble -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_ui_2exp"
        mpfr_set_ui_2exp :: Ptr MPFR -> CULong -> Exp -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_si_2exp"
        mpfr_set_si_2exp :: Ptr MPFR -> CLong -> Exp -> CRoundMode -> IO CInt


foreign import ccall unsafe "mpfr_set_str"
        mpfr_set_str :: Ptr MPFR -> CString -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_strtofr"
        mpfr_strtofr :: Ptr MPFR  ->  CString -> Ptr (Ptr CChar) -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_inf"
        mpfr_set_inf :: Ptr MPFR -> CInt -> IO ()

foreign import ccall unsafe "mpfr_set_nan"
        mpfr_set_nan :: Ptr MPFR -> IO ()

foreign import ccall unsafe "mpfr_swap"
        mpfr_swap :: Ptr MPFR -> Ptr MPFR -> IO ()

--------------------------------------------------------------------------------


-- conversion functions
foreign import ccall unsafe "mpfr_get_d"
        mpfr_get_d :: Ptr MPFR -> CRoundMode -> IO CDouble

foreign import ccall unsafe "mpfr_get_d_2exp"
        mpfr_get_d_2exp :: Ptr CLong -> Ptr MPFR -> CRoundMode -> IO CDouble

-- !!!!!!! next 4 set erange flags
foreign import ccall unsafe "mpfr_get_si"
        mpfr_get_si :: Ptr MPFR -> CRoundMode -> IO CLong

foreign import ccall unsafe "mpfr_get_ui"
        mpfr_get_ui :: Ptr MPFR -> CRoundMode -> IO CULong

foreign import ccall unsafe "mpfr_get_str"
        mpfr_get_str :: CString -> Ptr Exp -> CInt -> CUInt -> Ptr MPFR ->  CRoundMode -> IO CString

foreign import ccall unsafe "mpfr_free_str"
        mpfr_free_str :: CString -> IO ()

foreign import ccall unsafe "mpfr_fits_ulong_p"
        mpfr_fits_ulong_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_slong_p"
        mpfr_fits_slong_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_uint_p"
        mpfr_fits_uint_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_sint_p"
        mpfr_fits_sint_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_ushort_p"
        mpfr_fits_ushort_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_sshort_p"
        mpfr_fits_sshort_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_intmax_p"
        mpfr_fits_intmax_p :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fits_uintmax_p"
        mpfr_fits_uintmax_p :: Ptr MPFR -> CRoundMode -> IO CInt


-------------------------------------------------------------------------------

-- basic arithmetic functions

foreign import ccall unsafe "mpfr_add"
        mpfr_add :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_add_ui"
        mpfr_add_ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_add_si"
        mpfr_add_si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_add_d"
        mpfr_add_d :: Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub"
        mpfr_sub :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_sub"
        mpfr_ui_sub :: Ptr MPFR -> CULong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub_ui"
        mpfr_sub_ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_si_sub"
        mpfr_si_sub :: Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub_si"
        mpfr_sub_si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub_d"
        mpfr_sub_d :: Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_d_sub"
        mpfr_d_sub :: Ptr MPFR -> CDouble -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul"
        mpfr_mul :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_ui"
        mpfr_mul_ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_si"
        mpfr_mul_si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_d"
        mpfr_mul_d:: Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sqr"
        mpfr_sqr :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div"
        mpfr_div :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_div"
        mpfr_ui_div :: Ptr MPFR -> CULong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_ui"
        mpfr_div_ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_si_div"
        mpfr_si_div :: Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_si"
        mpfr_div_si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_d"
        mpfr_div_d :: Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_d_div"
        mpfr_d_div :: Ptr MPFR -> CDouble -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sqrt"
        mpfr_sqrt :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sqrt_ui"
        mpfr_sqrt_ui :: Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_rec_sqrt"
        mpfr_rec_sqrt :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cbrt"
        mpfr_cbrt :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_root"
        mpfr_root :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_pow"
        mpfr_pow :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_pow_ui"
        mpfr_pow_ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_pow_si"
        mpfr_pow_si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_pow_ui"
        mpfr_ui_pow_ui :: Ptr MPFR -> CULong -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_pow"
        mpfr_ui_pow :: Ptr MPFR -> CULong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_neg"
        mpfr_neg :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_abs_wrap"
        mpfr_abs :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_dim"
        mpfr_dim :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_2ui"
        mpfr_mul_2ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_2si"
        mpfr_mul_2si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_2ui"
        mpfr_div_2ui :: Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_2si"
        mpfr_div_2si :: Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt



--------------------------------------------------------------------------------
-- comparison functions
-- !!!!!!!! these set erange flags
foreign import ccall unsafe "mpfr_cmp_wrap"
        mpfr_cmp :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_cmp_ui_wrap"
        mpfr_cmp_ui :: Ptr MPFR -> CULong -> IO CInt

foreign import ccall unsafe "mpfr_cmp_si_wrap"
        mpfr_cmp_si :: Ptr MPFR -> CLong -> IO CInt

foreign import ccall unsafe "mpfr_cmp_d"
        mpfr_cmp_d :: Ptr MPFR -> CDouble -> IO CInt

foreign import ccall unsafe "mpfr_cmp_ui_2exp"
        mpfr_cmp_ui_2exp :: Ptr MPFR -> CULong -> Exp -> IO CInt

foreign import ccall unsafe "mpfr_cmp_si_2exp"
        mpfr_cmp_si_2exp :: Ptr MPFR -> CLong -> Exp -> IO CInt

foreign import ccall unsafe "mpfr_cmpabs"
        mpfr_cmpabs :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_nan_p_wrap"
        mpfr_nan_p :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_inf_p_wrap"
        mpfr_inf_p :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_number_p"
        mpfr_number_p :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_zero_p_wrap"
        mpfr_zero_p :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_sgn_wrap"
        mpfr_sgn :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_greater_p"
        mpfr_greater_p :: Ptr MPFR ->  Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_greaterequal_p"
        mpfr_greaterequal_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_less_p"
        mpfr_less_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_lessequal_p"
        mpfr_lessequal_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_lessgreater_p"
        mpfr_lessgreater_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_equal_p"
        mpfr_equal_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_unordered_p"
        mpfr_unordered_p :: Ptr MPFR -> Ptr MPFR -> IO CInt

-- special functions

foreign import ccall unsafe "mpfr_log"
        mpfr_log :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log2"
        mpfr_log2 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log10"
        mpfr_log10 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp"
        mpfr_exp :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp2"
        mpfr_exp2 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp10"
        mpfr_exp10 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sin"
        mpfr_sin :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cos"
        mpfr_cos :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_tan"
        mpfr_tan :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sec"
        mpfr_sec :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_csc"
        mpfr_csc :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cot"
        mpfr_cot :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sin_cos"
        mpfr_sin_cos :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_asin"
        mpfr_asin :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_acos"
        mpfr_acos :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_atan"
        mpfr_atan :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_atan2"
        mpfr_atan2 :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cosh"
        mpfr_cosh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sinh"
        mpfr_sinh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_tanh"
        mpfr_tanh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sinh_cosh"
        mpfr_sinh_cosh :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sech"
        mpfr_sech :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_csch"
        mpfr_csch :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_coth"
        mpfr_coth :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_asinh"
        mpfr_asinh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_acosh"
        mpfr_acosh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_atanh"
        mpfr_atanh :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fac_ui"
        mpfr_fac_ui :: Ptr MPFR -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log1p"
        mpfr_log1p :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_expm1"
        mpfr_expm1 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_li2"
        mpfr_li2 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_eint"
        mpfr_eint :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_gamma"
        mpfr_gamma :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_lngamma"
        mpfr_lngamma :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_lgamma"
        mpfr_lgamma :: Ptr MPFR -> Ptr CInt -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_digamma"
        mpfr_digamma :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_zeta"
        mpfr_zeta :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_zeta_ui"
        mpfr_zeta_ui :: Ptr MPFR -> CULong ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_erf"
        mpfr_erf :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_erfc"
        mpfr_erfc :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_j0"
        mpfr_j0 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_j1"
        mpfr_j1 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_jn"
        mpfr_jn :: Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_y0"
        mpfr_y0 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_y1"
        mpfr_y1 :: Ptr MPFR -> Ptr MPFR ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_yn"
        mpfr_yn :: Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fma"
        mpfr_fma :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fms"
        mpfr_fms :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_agm"
        mpfr_agm :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_hypot"
        mpfr_hypot :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

-- constants
foreign import ccall unsafe "mpfr_const_log2_wrap"
        mpfr_const_log2 :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_pi_wrap"
        mpfr_const_pi :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_euler_wrap"
        mpfr_const_euler :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_catalan_wrap"
        mpfr_const_catalan :: Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_free_cache"
        mpfr_free_cache :: IO ()

foreign import ccall unsafe "mpfr_sum"
        mpfr_sum :: Ptr MPFR -> Ptr (Ptr MPFR) -> CULong -> CRoundMode -> IO CInt

-- TODO input and output functions

-- integer related functions

foreign import ccall unsafe "mpfr_rint"
        mpfr_rint :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ceil_wrap"
        mpfr_ceil :: Ptr MPFR -> Ptr MPFR  -> IO CInt

foreign import ccall unsafe "mpfr_floor_wrap"
        mpfr_floor :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_round_wrap"
        mpfr_round :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_trunc_wrap"
        mpfr_trunc :: Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_rint_ceil"
        mpfr_rint_ceil :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_rint_floor"
        mpfr_rint_floor :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_rint_round"
        mpfr_rint_round :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_rint_trunc"
        mpfr_rint_trunc :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_frac"
        mpfr_frac :: Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_modf"
        mpfr_modf :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fmod"
        mpfr_fmod :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_remainder"
        mpfr_remainder :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_remquo"
        mpfr_remquo :: Ptr MPFR -> Ptr CLong -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_integer_p"
        mpfr_integer_p :: Ptr MPFR -> IO CInt

--------------------
-- miscellaneus functions

foreign import ccall unsafe "mpfr_nexttoward"
        mpfr_nexttoward ::  Ptr MPFR -> Ptr MPFR -> IO ()

foreign import ccall unsafe "mpfr_nextabove"
        mpfr_nextabove ::  Ptr MPFR -> IO ()

foreign import ccall unsafe "mpfr_nextbelow"
        mpfr_nextbelow ::  Ptr MPFR -> IO ()

foreign import ccall unsafe "mpfr_min"
        mpfr_min :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_max"
        mpfr_max :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_urandomb"
        mpfr_urandomb :: Ptr MPFR -> Ptr GmpRandState -> IO ()

foreign import ccall unsafe "mpfr_get_exp_wrap"
        mpfr_get_exp :: Ptr MPFR -> IO Exp

foreign import ccall unsafe "mpfr_set_exp"
        mpfr_set_exp :: Ptr MPFR -> Exp -> IO CInt

foreign import ccall unsafe "mpfr_signbit_wrap"
        mpfr_signbit :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_setsign_wrap"
        mpfr_setsign :: Ptr MPFR -> Ptr MPFR -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_copysign_wrap"
        mpfr_copysign :: Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt

---------------------------------------------------------------
-- rounding mode related functions

foreign import ccall unsafe "mpfr_get_emin"
        mpfr_get_emin :: IO Exp

foreign import ccall unsafe "mpfr_get_emax"
        mpfr_get_emax :: IO Exp

foreign import ccall unsafe "mpfr_set_emin"
        mpfr_set_emin :: Exp -> IO CInt

foreign import ccall unsafe "mpfr_set_emax"
        mpfr_set_emax :: Exp -> IO CInt

foreign import ccall unsafe "mpfr_get_emin_min"
        mpfr_get_emin_min :: IO Exp

foreign import ccall unsafe "mpfr_get_emin_max"
        mpfr_get_emin_max :: IO Exp

foreign import ccall unsafe "mpfr_get_emax_min"
        mpfr_get_emax_min :: IO Exp

foreign import ccall unsafe "mpfr_get_emax_max"
        mpfr_get_emax_max :: IO Exp

foreign import ccall unsafe "mpfr_check_range"
        mpfr_check_range :: Ptr MPFR -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_subnormalize"
        mpfr_subnormalize :: Ptr MPFR -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_clear_underflow"
        mpfr_clear_underflow :: IO ()

foreign import ccall unsafe "mpfr_clear_overflow"
        mpfr_clear_overflow :: IO ()

foreign import ccall unsafe "mpfr_clear_nanflag"
        mpfr_clear_nanflag :: IO ()

foreign import ccall unsafe "mpfr_clear_inexflag"
        mpfr_clear_inexflag :: IO ()

foreign import ccall unsafe "mpfr_clear_erangeflag"
        mpfr_clear_erangeflag :: IO ()

foreign import ccall unsafe "mpfr_set_underflow"
        mpfr_set_underflow :: IO ()

foreign import ccall unsafe "mpfr_set_overflow"
        mpfr_set_overflow :: IO ()

foreign import ccall unsafe "mpfr_set_nanflag"
        mpfr_set_nanflag :: IO ()

foreign import ccall unsafe "mpfr_set_inexflag"
        mpfr_set_inexflag :: IO ()

foreign import ccall unsafe "mpfr_set_erangeflag"
        mpfr_set_erangeflag :: IO ()

foreign import ccall unsafe "mpfr_clear_flags"
        mpfr_clear_flags :: IO ()

foreign import ccall unsafe "mpfr_underflow_p"
        mpfr_underflow_p :: IO CInt

foreign import ccall unsafe "mpfr_overflow_p"
        mpfr_overflow_p :: IO CInt

foreign import ccall unsafe "mpfr_nanflag_p"
        mpfr_nanflag_p :: IO CInt

foreign import ccall unsafe "mpfr_inexflag_p"
        mpfr_inexflag_p :: IO CInt

foreign import ccall unsafe "mpfr_erangeflag_p"
        mpfr_erangeflag_p :: IO CInt

---------------------------------------------------------------
-- custom interface
foreign import ccall unsafe "mpfr_custom_get_size_wrap"
        mpfr_custom_get_size :: CPrecision -> IO Word64
{-# LINE 790 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

foreign import ccall unsafe "mpfr_custom_init_wrap"
        mpfr_custom_init :: Ptr Word64 -> CPrecision -> IO ()
{-# LINE 793 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

foreign import ccall unsafe "mpfr_custom_init_set_wrap"
        mpfr_custom_init_set :: Ptr MPFR -> CInt -> Exp -> CPrecision -> Ptr Limb -> IO ()

foreign import ccall unsafe "mpfr_custom_get_kind_wrap"
        mpfr_custom_get_kind :: Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_custom_get_mantissa_wrap"
        mpfr_custom_get_mantissa :: Ptr MPFR -> IO (Ptr Limb)

foreign import ccall unsafe "mpfr_custom_get_exp_wrap"
        mpfr_custom_get_exp :: Ptr MPFR -> IO Exp

foreign import ccall unsafe "mpfr_custom_move_wrap"
        mpfr_custom_move :: Ptr MPFR -> Ptr Word64 -> IO ()
{-# LINE 808 "src/Data/Number/MPFR/FFIhelper.hsc" #-}

-------------------------------------------------
