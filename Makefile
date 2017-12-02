default:
	corebuild -pkg async main.byte && ./main.byte

clean:
	corebuild -clean