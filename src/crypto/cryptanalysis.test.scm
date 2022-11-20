;; This file is part of alchemy.
;; Copyright (c) 2022 unpx.net
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(import (alchemy cauldron)
        (alchemy cryptanalysis))

(steer-observe
  "Cryptopals 1.3."
  (car (attack-single-xor-cipher-english
    (decode-hex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")))
  "Cooking MC's like a pound of bacon")

(steer-observe
  "Cryptopals 1.5."
  (xor-key
    #u8"ICE"
    #u8"Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
  (decode-hex "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"))


; (steer "Count freq"
;        (begin
;          (display "\n")
;          (display (length (freq-count "V, s kes, mvoj kkdlof ijidd rdseoxw z osfl hpgt ,foo ;wt gu jke rdaror .tpo ;kwpc kdr Kncl G brezols tsr jf;m ,oq.d; tb.f uf;hpxg ffjumr tddk ,bppvsp kh;cp jd ;cd nc zuzglw G pvcd oe mdpg s; znalyd ldka jxphblu inglu; ijso wkd fpfh ks owr,fps sknci mufli;s Bg gsv wkr sozc;j mjxo H ydm uzrjgocya gaf ,yin, ,wpk ke lczo lxjc dwi e;tule")))))
