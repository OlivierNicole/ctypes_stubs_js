(module
   (import "env" "Double_val"
      (func $Double_val (param (ref eq)) (result f64)))
   (import "env" "caml_format_int"
      (func $caml_format_int
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_blit_string"
      (func $caml_blit_string
         (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq))
         (result (ref eq))))
   (import "env" "caml_format_float"
      (func $caml_format_float (param (ref eq) (ref eq)) (result (ref eq))))
   (import "env" "caml_float_of_string"
      (func $caml_float_of_string (param (ref eq)) (result (ref eq))))
   (import "Math" "exp" (func $exp (param f64) (result f64)))
   (import "Math" "log" (func $log (param f64) (result f64)))
   (import "Math" "log10" (func $log10 (param f64) (result f64)))
   (import "Math" "expm1" (func $expm1 (param f64) (result f64)))
   (import "Math" "log1p" (func $log1p (param f64) (result f64)))
   (import "Math" "cos" (func $cos (param f64) (result f64)))
   (import "Math" "sin" (func $sin (param f64) (result f64)))
   (import "Math" "tan" (func $tan (param f64) (result f64)))
   (import "Math" "acos" (func $acos (param f64) (result f64)))
   (import "Math" "asin" (func $asin (param f64) (result f64)))
   (import "Math" "atan" (func $atan (param f64) (result f64)))
   (import "Math" "cosh" (func $cosh (param f64) (result f64)))
   (import "Math" "sinh" (func $sinh (param f64) (result f64)))
   (import "Math" "tanh" (func $tanh (param f64) (result f64)))
   (import "Math" "acosh" (func $acosh (param f64) (result f64)))
   (import "Math" "asinh" (func $asinh (param f64) (result f64)))
   (import "Math" "atanh" (func $atanh (param f64) (result f64)))
   (import "Math" "pow" (func $pow (param f64 f64) (result f64)))
   (import "Math" "atan2" (func $atan2 (param f64 f64) (result f64)))
   (import "Math" "hypot" (func $hypot (param f64 f64) (result f64)))
   (import "env" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "env" "REMAINDER_ERRMSG" (global $REMAINDER_ERRMSG (ref $string)))

   (func (export "ldouble_init") (param (ref eq)) (result (ref eq))
      ;; Do nothing
      (ref.i31 (i32.const 0)))

   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   ;; We implement C's 'long double' as a double-precision float.
   (func (export "ctypes_ldouble_of_float")
      (param $v (ref eq)) (result (ref eq))
      ;; Check that the argument is a $float
      (ref.cast (ref $float) (local.get $v)))

   (func (export "ctypes_ldouble_to_float")
      (param $v (ref eq)) (result (ref eq))
      ;; Check that the argument is a $float
      (ref.cast (ref $float) (local.get $v)))

   (func (export "ctypes_ldouble_to_int")
      (param $v (ref eq)) (result (ref eq))
      (local $f f64)
      (local.set $f (call $Double_val (local.get $v)))
      (ref.i31 (i32.trunc_sat_f64_s (local.get $f))))

   (func (export "ctypes_ldouble_of_int")
      (param $v (ref eq)) (result (ref eq))
      (struct.new $float
         (f64.convert_i32_s (i31.get_s (ref.cast (ref i31) (local.get $v))))))

   (global $PERCENT_D (ref $string) (array.new_fixed $string 2
         (i32.const 0x25) (i32.const 0x68))) ;; "%d"

   (func (export "ctypes_ldouble_format")
      (param $width (ref eq)) (param $prec (ref eq)) (param $v (ref eq))
      (result (ref eq))
      (local $width_str (ref $string))
      (local $prec_str (ref $string))
      (local $format_string_len i32)
      (local $format_string (ref $string))
      (local.set $width_str
         (ref.cast (ref $string)
            (call $caml_format_int (global.get $PERCENT_D)
                                   (ref.cast (ref i31) (local.get $width)))))
      (local.set $prec_str
         (ref.cast (ref $string)
            (call $caml_format_int (global.get $PERCENT_D)
                                   (ref.cast (ref i31) (local.get $prec)))))
      ;; Construct the format string ["%" ^ $width_str ^ "." ^ $prec_str ^ "f"
      (local.set $format_string_len
         (i32.add (i32.add (array.len (local.get $width_str))
                           (array.len (local.get $prec_str)))
                  (i32.const 3)))
      (local.set $format_string
         (array.new $string
                    (i32.const 0x25) ;; '%'
                    (local.get $format_string_len)))
      (drop (call $caml_blit_string
               (local.get $width_str)
               (ref.i31 (i32.const 0))
               (local.get $format_string)
               (ref.i31 (i32.const 1))
               (ref.i31 (array.len (local.get $width_str)))))
      (array.set $string (local.get $format_string)
         (i32.add (array.len (local.get $width_str)) (i32.const 1))
         (i32.const 0x2e)) ;; '.'
      (drop (call $caml_blit_string
               (local.get $prec_str)
               (ref.i31 (i32.const 0))
               (local.get $format_string)
               (ref.i31 (i32.add (array.len (local.get $width_str)) (i32.const 1)))
               (ref.i31 (array.len (local.get $prec_str)))))
      (array.set $string (local.get $format_string)
         (i32.sub (local.get $format_string_len) (i32.const 1))
         (i32.const 0x66)) ;; 'f'
      (return_call $caml_format_float
         (local.get $format_string)
         (local.get $v)))

   (func (export "ctypes_ldouble_of_string") (param $v (ref eq)) (result (ref eq))
      (return_call $caml_float_of_string (local.get $v)))

   (func (export "ctypes_ldouble_add")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (f64.add (call $Double_val (local.get $x))
                                  (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_sub")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (f64.sub (call $Double_val (local.get $x))
                                  (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_mul")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (f64.mul (call $Double_val (local.get $x))
                                  (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_div")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (f64.div (call $Double_val (local.get $x))
                                  (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_neg")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (f64.neg (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_sqrtl")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (f64.sqrt (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_expl")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $exp (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_logl")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $log (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_log10l")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $log10 (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_expm1l")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $expm1 (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_log1pl")
      (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $log1p (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_cosl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $cos (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_sinl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $sin (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_tanl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $tan (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_acosl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $acos (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_asinl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $asin (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_atanl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $atan (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_coshl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $cosh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_sinhl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $sinh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_tanhl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $tanh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_acoshl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $acosh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_asinhl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $asinh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_atanhl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (call $atanh (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_ceill") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (f64.ceil (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_floorl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (f64.floor (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_fabsl") (param $x (ref eq)) (result (ref eq))
      (struct.new $float (f64.abs (call $Double_val (local.get $x)))))

   (func (export "ctypes_ldouble_powl")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (call $pow (call $Double_val (local.get $x))
                                    (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_atan2l")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (call $atan2 (call $Double_val (local.get $x))
                                      (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_hypotl")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (struct.new $float (call $hypot (call $Double_val (local.get $x))
                                      (call $Double_val (local.get $y)))))

   (func (export "ctypes_ldouble_remainderl") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $REMAINDER_ERRMSG))
      (ref.i31 (i32.const 0)))

   (func (export "ctypes_ldouble_copysignl")
      (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
      (local $a f64)
      (local $b f64)
      (local.set $a (call $Double_val (local.get $v1)))
      (local.set $b (call $Double_val (local.get $v2)))
      (if (f64.lt (local.get $b) (f64.const 0))
         (then (return (struct.new $float (f64.neg (f64.abs (local.get $a))))))
         (else (return (struct.new $float (f64.abs (local.get $a)))))))
)
