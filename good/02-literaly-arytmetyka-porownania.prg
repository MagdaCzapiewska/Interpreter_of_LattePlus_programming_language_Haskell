int dodaj(int x, int y) {
	return x + y;
}
int odejmij(int x, int y) {
	return x - y;
}
int pomnoz(int x, int y) {
	return x * y;
}
int podziel(int x, int y) {
	return x / y;
}
int modulo(int x, int y) {
	return x % y;
}
int negacja(int x) {
	return -x;
}
boolean wieksze(int x, int y) {
	return x > y;
}
boolean wieksze_rowne(int x, int y) {
	return x >= y;
}
boolean mniejsze(int x, int y) {
	return x < y;
}
boolean mniejsze_rowne(int x, int y) {
	return x <= y;
}
boolean rowne(int x, int y) {
	return x == y;
}
boolean rozne(int x, int y) {
	return x != y;
}
boolean negacja_logiczna(boolean x) {
	return !x;
}

printInt(dodaj(3*10, 10));
printInt(odejmij(99*100, 100));
printInt(pomnoz(7-3, 2));
printInt(podziel(7, 3));
printInt(modulo(7, 3));
printInt(negacja(5));
printBool(wieksze(100, 99));
printBool(wieksze_rowne(100, 99));
printBool(mniejsze(99, 100));
printBool(mniejsze_rowne(100, 99));
printBool(rowne(100, 99));
printBool(rozne(100, 99));
printBool(negacja_logiczna(true));
