(module
   (type $string (array (mut i8)))

   ;; "ctypes: remainderl does not exist on current platform"
   (global $REMAINDER_ERRMSG (export "REMAINDER_ERRMSG")
      (ref $string) (array.new_fixed $string
      (i32.const 0x63) (i32.const 0x74) (i32.const 0x79) (i32.const 0x70)
      (i32.const 0x65) (i32.const 0x73) (i32.const 0x3a) (i32.const 0x20)
      (i32.const 0x72) (i32.const 0x65) (i32.const 0x6d) (i32.const 0x61)
      (i32.const 0x69) (i32.const 0x6e) (i32.const 0x64) (i32.const 0x65)
      (i32.const 0x72) (i32.const 0x6c) (i32.const 0x20) (i32.const 0x64)
      (i32.const 0x6f) (i32.const 0x65) (i32.const 0x73) (i32.const 0x20)
      (i32.const 0x6e) (i32.const 0x6f) (i32.const 0x74) (i32.const 0x20)
      (i32.const 0x65) (i32.const 0x78) (i32.const 0x69) (i32.const 0x73)
      (i32.const 0x74) (i32.const 0x20) (i32.const 0x6f) (i32.const 0x6e)
      (i32.const 0x20) (i32.const 0x63) (i32.const 0x75) (i32.const 0x72)
      (i32.const 0x72) (i32.const 0x65) (i32.const 0x6e) (i32.const 0x74)
      (i32.const 0x20) (i32.const 0x70) (i32.const 0x6c) (i32.const 0x61)
      (i32.const 0x74) (i32.const 0x66) (i32.const 0x6f) (i32.const 0x72)
      (i32.const 0x6d)))

)
