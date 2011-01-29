import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma._
import org.tearne.beaner.report._

PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))

val p1 = PhaseolusVulgaris()
val p2 = PhaseolusVulgaris()
val p3 = PhaseolusVulgaris()
val p4 = PhaseolusVulgaris()
val pV = PhaseolusVulgaris()

val c1 = new Criterion(p1, 1, 9)
val c2 = new Criterion(p2, 3, 50)
val c3 = new Criterion(p3, 4, 24)
val c4 = new Criterion(p4, 7, 36)
val cAll = c1 + c2 + c3 + c4

val f1_p1p2 = p1 x p2 selectHet c1 + c2
val f1_p3p4 = p3 x p4 selectHet c3 + c4
val f1_p1p2p3p4 = f1_p1p2 x f1_p3p4 selectHet cAll

val bc1 = f1_p1p2p3p4 x pV selectHet cAll
val bc2 = bc1 x pV selectHet cAll
val bc3 = bc2 x pV selectHet cAll
val bc4 = bc3 x pV selectHet cAll

val fin = bc4 x bc4 selectHom cAll

//Make colour object
val colours = new Colour(cAll, pV)

val plantsList = List(
  NamedPlant(pV, "Pref Var"),
  NamedPlant(p1, "First Donor"),
  NamedPlant(p2, "Second Donor"),
  NamedPlant(p3, "Third Donor"),
  NamedPlant(p4, "Fourth Donor"),
  NamedPlant(f1_p1p2, "F1 (p1 x p2)"),
  NamedPlant(f1_p3p4, "F1 (p3 x p4)"),
  NamedPlant(f1_p1p2p3p4, "F1 ((p1 x p2) x (p3 x p4))"),
  NamedPlant(bc1, "Backcross 1"),
  NamedPlant(bc2, "Backcross 2"),
  NamedPlant(bc3, "Backcross 3"),
  NamedPlant(bc4, "Backcross 4"),
  NamedPlant(fin, "Selfed")
)

new Reporter(plantsList, cAll, colours).makePDF()

Thread.sleep(10000)
