boolean rozne_naokolo(int x, int y) {
	ifel (x > y) {
		return true;
	}
	else {
		ifel (x < y) {
			return true;
		}
		else {
			return false;
		}
	}
	return false;
}

printBool(rozne_naokolo(5, 6));
printBool(rozne_naokolo(6, 5));
printBool(rozne_naokolo(5, 5));
