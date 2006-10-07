;;; test cases (SRFI-78) for SHA-1
;;; 
;;; taken from:
;;;   Federal Information Processing Standard Publication 180-1
;;;   Secure Hash Standard
;;;   April 1995
;;; available from
;;;    <http://www.itl.nist.gov/fipspubs/fip180-1.htm>

(check 
 (sha1-hash-string "")
 => #xda39a3ee5e6b4b0d3255bfef95601890afd80709)

(check 
 (sha1-hash-string "a")
 => #x86f7e437faa5a7fce15d1ddcb9eaeaea377667b8)

(check 
 (sha1-hash-string "abc")
 => #xa9993e364706816aba3e25717850c26c9cd0d89d)

(check 
 (sha1-hash-string "message digest")
 => #xc12252ceda8be8994d5fa0290a47231c1d16aae3)

(check 
 (sha1-hash-string "abcdefghijklmnopqrstuvwxyz")
 => #x32d10c7b8cf96570ca04ce37f2a19d84240d3a89)

(check 
 (sha1-hash-string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
 => #x761c457bf73b14d27e9e9265c46f4b4dda11f940)

(check 
 (sha1-hash-string "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
 => #x50abf5706a150990a08b2c5ea40fa0e585554732)

(check 
 (sha1-hash-string "MD5 has not yet (2001-09-03) been broken, but sufficient attacks have been made that its security is in some doubt")
 => #x18eca4333979c4181199b7b4fab8786d16cf2846)

(check
 (sha1-hash-string "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
 => #x84983e441c3bd26ebaae4aa1f95129e5e54670f1)

;;; run this test only if you want to burn many cpu cycles...
;(check
; (sha1-hash-string (make-string 1000000 #\a))
; => #x34aa973cd4c4daa4f61eeb2bdbad27316534016f)
