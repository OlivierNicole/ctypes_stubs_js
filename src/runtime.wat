(module
   (import "env" "Double_val"
      (func $Double_val (param (ref eq)) (result f64)))

   (func (export "ldouble_init") (param (ref eq)) (result (ref eq))
      ;; Do nothing
      (ref.i31 (i32.const 0)))

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
)
