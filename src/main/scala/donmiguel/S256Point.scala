package donmiguel

class S256Point(_x: Element, _y: Element) extends Point(Some(_x), Some(_y), new S256Element(Secp256k1.A), new S256Element(Secp256k1.B)) {

}
