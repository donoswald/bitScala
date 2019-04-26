package donmiguel.script

object OpCode extends Enumeration {

  val OP_0 = Value(0)
  val OP_FALSE = OP_0

  val OP_DATA_1 = Value(1)
  val OP_DATA_2 = Value(2)
  val OP_DATA_3 = Value(3)
  val OP_DATA_4 = Value(4)
  val OP_DATA_5 = Value(5)
  val OP_DATA_6 = Value(6)
  val OP_DATA_7 = Value(7)
  val OP_DATA_8 = Value(8)
  val OP_DATA_9 = Value(9)

  val OP_DATA_10 = Value(10)
  val OP_DATA_11 = Value(11)
  val OP_DATA_12 = Value(12)
  val OP_DATA_13 = Value(13)
  val OP_DATA_14 = Value(14)
  val OP_DATA_15 = Value(15)
  val OP_DATA_16 = Value(16)
  val OP_DATA_17 = Value(17)
  val OP_DATA_18 = Value(18)
  val OP_DATA_19 = Value(19)

  val OP_DATA_20 = Value(20)
  val OP_DATA_21 = Value(21)
  val OP_DATA_22 = Value(22)
  val OP_DATA_23 = Value(23)
  val OP_DATA_24 = Value(24)
  val OP_DATA_25 = Value(25)
  val OP_DATA_26 = Value(26)
  val OP_DATA_27 = Value(27)
  val OP_DATA_28 = Value(28)
  val OP_DATA_29 = Value(29)

  val OP_DATA_30 = Value(30)
  val OP_DATA_31 = Value(31)
  val OP_DATA_32 = Value(32)
  val OP_DATA_33 = Value(33)
  val OP_DATA_34 = Value(34)
  val OP_DATA_35 = Value(35)
  val OP_DATA_36 = Value(36)
  val OP_DATA_37 = Value(37)
  val OP_DATA_38 = Value(38)
  val OP_DATA_39 = Value(39)

  val OP_DATA_40 = Value(40)
  val OP_DATA_41 = Value(41)
  val OP_DATA_42 = Value(42)
  val OP_DATA_43 = Value(43)
  val OP_DATA_44 = Value(44)
  val OP_DATA_45 = Value(45)
  val OP_DATA_46 = Value(46)
  val OP_DATA_47 = Value(47)
  val OP_DATA_48 = Value(48)
  val OP_DATA_49 = Value(49)

  val OP_DATA_50 = Value(50)
  val OP_DATA_51 = Value(51)
  val OP_DATA_52 = Value(52)
  val OP_DATA_53 = Value(53)
  val OP_DATA_54 = Value(54)
  val OP_DATA_55 = Value(55)
  val OP_DATA_56 = Value(56)
  val OP_DATA_57 = Value(57)
  val OP_DATA_58 = Value(58)
  val OP_DATA_59 = Value(59)

  val OP_DATA_60 = Value(60)
  val OP_DATA_61 = Value(61)
  val OP_DATA_62 = Value(62)
  val OP_DATA_63 = Value(63)
  val OP_DATA_64 = Value(64)
  val OP_DATA_65 = Value(65)
  val OP_DATA_66 = Value(66)
  val OP_DATA_67 = Value(67)
  val OP_DATA_68 = Value(68)
  val OP_DATA_69 = Value(69)

  val OP_DATA_70 = Value(70)
  val OP_DATA_71 = Value(71)
  val OP_DATA_72 = Value(72)
  val OP_DATA_73 = Value(73)
  val OP_DATA_74 = Value(74)
  val OP_DATA_75 = Value(75)

  val OP_PUSHDATA1 = Value(76)
  val OP_PUSHDATA2 = Value(77)
  val OP_PUSHDATA4 = Value(78)

  val OP_1NEGATE = Value(79)
  val OP_1 = Value(81)
  val OP_TRUE = OP_1
  val OP_2 = Value(82)
  val OP_3 = Value(83)
  val OP_4 = Value(84)
  val OP_5 = Value(85)
  val OP_6 = Value(86)
  val OP_7 = Value(87)
  val OP_8 = Value(88)
  val OP_9 = Value(89)
  val OP_10 = Value(90)
  val OP_11 = Value(91)
  val OP_12 = Value(92)
  val OP_13 = Value(93)
  val OP_14 = Value(94)
  val OP_15 = Value(95)
  val OP_16 = Value(96)

  // control
  val OP_NOP = Value(97)
  val OP_IF = Value(99)
  val OP_NOTIF = Value(100)
  val OP_ELSE = Value(103)
  val OP_ENDIF = Value(104)
  val OP_VERIFY = Value(105)
  val OP_RETURN = Value(106)

  // stack ops
  val OP_TOALTSTACK = Value(107)
  val OP_FROMALTSTACK = Value(108)
  val OP_2DROP = Value(109)
  val OP_2DUP = Value(110)
  val OP_3DUP = Value(111)
  val OP_2OVER = Value(112)
  val OP_2ROT = Value(113)
  val OP_2SWAP = Value(114)
  val OP_IFDUP = Value(115)
  val OP_DEPTH = Value(116)
  val OP_DROP = Value(117)
  val OP_DUP = Value(118)
  val OP_NIP = Value(119)
  val OP_OVER = Value(120)
  val OP_PICK = Value(121)
  val OP_ROLL = Value(122)
  val OP_ROT = Value(123)
  val OP_SWAP = Value(124)
  val OP_TUCK = Value(125)


  // splice ops
  @OpCodeDisabled
  val OP_CAT = Value(126)
  @OpCodeDisabled
  val OP_SUBSTR = Value(127)
  @OpCodeDisabled
  val OP_LEFT = Value(128)
  @OpCodeDisabled
  val OP_RIGHT = Value(129)
  val OP_SIZE = Value(130)

  // bit logic
  @OpCodeDisabled
  val OP_INVERT = Value(131)
  @OpCodeDisabled
  val OP_AND = Value(132)
  @OpCodeDisabled
  val OP_OR = Value(133)
  @OpCodeDisabled
  val OP_XOR = Value(134)
  val OP_EQUAL = Value(135)
  val OP_EQUALVERIFY = Value(136)

  // numeric
  val OP_1ADD = Value(139)
  val OP_1SUB = Value(140)
  @OpCodeDisabled
  val OP_2MUL = Value(141)
  @OpCodeDisabled
  val OP_2DIV = Value(142)
  val OP_NEGATE = Value(143)
  val OP_ABS = Value(144)
  val OP_NOT = Value(145)
  val OP_0NOTEQUAL = Value(146)
  val OP_ADD = Value(147)
  val OP_SUB = Value(148)
  @OpCodeDisabled
  val OP_MUL = Value(149)
  @OpCodeDisabled
  val OP_DIV = Value(150)
  @OpCodeDisabled
  val OP_MOD = Value(151)
  @OpCodeDisabled
  val OP_LSHIFT = Value(152)
  @OpCodeDisabled
  val OP_RSHIFT = Value(153)
  val OP_BOOLAND = Value(154)
  val OP_BOOLOR = Value(155)
  val OP_NUMEQUAL = Value(156)
  val OP_NUMEQUALVERIFY = Value(157)
  val OP_NUMNOTEQUAL = Value(158)
  val OP_LESSTHAN = Value(159)
  val OP_GREATERTHAN = Value(160)
  val OP_LESSTHANOREQUAL = Value(161)
  val OP_GREATERTHANOREQUAL = Value(162)
  val OP_MIN = Value(163)
  val OP_MAX = Value(164)
  val OP_WITHIN = Value(165)

  // crypto
  val OP_RIPEMD160 = Value(166)
  val OP_SHA1 = Value(167)
  val OP_SHA256 = Value(168)
  val OP_HASH160 = Value(169)
  val OP_HASH256 = Value(170)
  val OP_CODESEPARATOR = Value(171)
  val OP_CHECKSIG = Value(172)
  val OP_CHECKSIGVERIFY = Value(173)
  val OP_CHECKMULTISIG = Value(174)
  val OP_CHECKMULTISIGVERIFY = Value(175)

  // block state
  val OP_CHECKLOCKTIMEVERIFY = Value(177)
  val OP_CHECKSEQUENCEVERIFY = Value(178)

  // reserved words
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED = Value(80)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_VER = Value(98)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = false)
  val OP_VERIF = Value(101)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = false)
  val OP_VERNOTIF = Value(102)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED1 = Value(137)
  @OpCodeReserved(ignoreComplete = false, ignoreIfUnexecuted = true)
  val OP_RESERVED2 = Value(138)
  @OpCodeReserved
  val OP_NOP1 = Value(176)
  @OpCodeReserved
  val OP_NOP4 = Value(179)
  @OpCodeReserved
  val OP_NOP5 = Value(180)
  @OpCodeReserved
  val OP_NOP6 = Value(181)
  @OpCodeReserved
  val OP_NOP7 = Value(182)
  @OpCodeReserved
  val OP_NOP8 = Value(183)
  @OpCodeReserved
  val OP_NOP9 = Value(184)
  @OpCodeReserved
  val OP_NOP10 = Value(185)

  // pseudo words
  val OP_PUBKEYHASH = Value(253)
  val OP_PUBKEY = Value(254)
  val OP_INVALIDOPCODE = Value(255)

}
