package derifree.etd

enum Exchange(val mic: String):
  case CBOE extends Exchange("XCBO")
  case NASDAQ extends Exchange("XNAS")
