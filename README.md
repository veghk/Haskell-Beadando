Hét csoda párbaj
A feladat során a Hét csoda nevű társasjáték két fős változatát, a Hét csoda párbaj (Seven wonders duel) egy (jelentősen) egyszerűsített változatát fogjuk implementálni. Ez egy civilizáció építő játék, ahol a játékosok várost építenek a különféle kártyák segítségével, a párbajt pedig az erősebb város nyeri.

Mivel az implementációban nem lehet grafikai felületünk, ezért csak egy szimulációt fogunk elvégezni, vagyis önmaguktól fognak zajlani a körök.

Megjegyzések és előfeltételek
Tesztesetek
A feladatokat a leírásnak megfelelően kell megoldani, de minden feladatnál adottak tesztesetek, amelyek a függvény működésének tesztelését segítik.
Tekintve, hogy a tesztesetek - bár odafigyelés mellett íródnak - nem fedik le minden esetben a függvény teljes működését, ezért határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt!
A végső teszteléshez használt tesztek halmaza bővebb lehet a megadott teszthalmaznál.
A játékról
Minden kártyának van egy költsége, azaz amennyiért lehet megvenni (Cost). Ez lehet pénz és/vagy nyersanyagok.
Ezt az eredeti szabály 5. oldala taglalja. (A beadandó szabályai eltérnek az eredetitől.)
Egy nyersanyag típus csak egyszer szerepel a Cost-ban.
Minden kártyának van egy hatása, ami típusonként eltér.
A játékban minden kártya pontosan egyszer található meg. Két kártya hatása lehet ugyanaz, de a nevük mindenképp különbözik.
A sima kártyák mellett vannak különleges ún. csoda kártyák (lásd bővebben lentebb).
A játékot két játékos játssza. A játékosok pénzzel, kártyákkal és csodákkal rendelkeznek.
A játékosnál levő kártyákat (a csoda kártyákat is ideértve) nevezzük városnak vagy a játékos kezében lévő kártyáknak.

