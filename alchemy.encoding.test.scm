(import (alchemy cauldron)
        (alchemy encoding))

(steer-observe "Base64 1 byte."  (encode-base64 #u8"M") "TQ==")
(steer-observe "Base64 2 bytes." (encode-base64 #u8"Ma") "TWE=")
(steer-observe "Base64 3 bytes." (encode-base64 #u8"Man") "TWFu")

(steer-observe
  "Cryptopals 1.1."
  (encode-base64 (decode-hex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
  "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

