package au.aossie

import org.scalameter.api._
import org.scalameter.picklers.Implicits.stringPickler

import au.aossie.scavenger.prover.{ CR, ProblemStatus, Unsatisfiable, Satisfiable }
import au.aossie.scavenger.expression._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import au.aossie.scavenger.structure.immutable.{CNF, SeqClause => Clause}

import au.aossie.scavenger.parser.TPTP.{CNFAxiomStatement, CNFNegatedConjectureStatement, CNFProblem, CNF => TPTPCNF}

import au.aossie.scavenger.parser.TPTPCNFParser

/**
 * Performance regression test on all easy unsatisfiable CNF problems of the TPTP library
 *
 * Before executing this test, the whole TPTP library must be downloaded and included in the "examples/problems/" folder.
 *
 */
object EasyTPTPCNFUnsatNoEq extends Bench.OfflineRegressionReport {
  // TODO: This is not the right place for this function
  // TODO: remove this function from here once we have a better way to distinguish constants and variables
  def getUppercaseVariables(cnf: CNF): mutable.Set[Sym] = {
    def uppercaseVariableInFormula(e: E): Set[Sym] = e match {
      case v: Sym if v.name.charAt(0).isUpper => Set(v)
      case App(l, r) => uppercaseVariableInFormula(l) ++ uppercaseVariableInFormula(r)
      case Abs(_,_, body) => uppercaseVariableInFormula(body)
      case _ => Set()
    }
    val variables = mutable.Set.empty[Sym]
    cnf.clauses.flatMap(clause => clause.ant ++ clause.suc).foreach(variables ++= uppercaseVariableInFormula(_))
    variables
  }

  def test(testName: String): ProblemStatus = {
    val p = TPTPCNFParser.parse("examples/problems/TPTP-v6.4.0/Problems/" +  testName.take(3) + "/" + testName + ".p")
    implicit val vars = getUppercaseVariables(p)
    CR.prove(p)
  }

  // TODO: Right now we are executing the test only on a subset. Enable the whole set when the prover has a timeout. Otherwise this test might never terminate.
//  val problems = Seq(//"ANA013-2","ANA025-2","ANA037-2","ANA038-2","ANA039-2","ANA041-2","ANA042-2", // these problems are ok.
                     //"CAT007-3","COL101-2","COL102-2","COL104-2","COL105-2","COL109-2","COL110-2", // ok
                     //"COL111-2","COL112-2","COL113-2","COL114-2","COL115-2","COL116-2","COL117-2", // ok
                     //"COL118-2","COL119-2","COL120-2","COL121-2","COL122-2","COL123-2","COL124-2", // ok
                     //"COM001-1","COM002-2","COM002-1", // ok
                     //"COM003-2","COM011-2", // ok
                     //"FLD001-3"."FLD002-3","FLD006-1", // timeout
                     //"FLD006-3", // ok
                     //"FLD010-5","FLD013-3","FLD013-4","FLD013-5","FLD014-3","FLD017-1", // timeout
                     //"FLD017-1", // timeout
                     //"FLD017-3", // ok
                     //"FLD018-3","FLD019-3","FLD021-3","FLD023-3","FLD024-3","FLD025-3","FLD025-4", // timeout
                     //"FLD025-5","FLD030-1", // ok
                     //"FLD030-2","FLD030-3", // timeout
                     //"FLD030-4", // ok
                     //"FLD034-3","FLD037-3", // timeout
                     //"FLD039-1","FLD039-3", // ok
                     //"FLD043-3","FLD055-1","FLD055-3", // timeout
                     //"FLD056-1","FLD056-3", // ok
                     //"FLD058-1","FLD058-3","FLD059-1","FLD059-3","FLD059-4", // timeout
                     //"FLD064-3","FLD065-3","FLD067-3","FLD067-4","FLD068-3","FLD068-4", // timeout
                     //"FLD069-3", // ok
                     //"FLD070-4", // timeout
                     //"FLD071-1","FLD071-4", // ok
                     //"GEO002-4", // timeout
                     //"GEO079-1","GRA001-1","GRP001-5","GRP003-1", // ok
                     //"GRP003-2", // timeout
                     //"GRP004-1","GRP004-2","GRP005-1","GRP006-1","GRP028-1","GRP028-3","GRP028-4", // ok
                     //"GRP029-2","GRP031-2","GRP033-3","GRP034-4","GRP041-2","GRP042-2","GRP043-2", // timeout
                     //"GRP044-2", // ok
                     //"GRP045-2","GRP046-2","GRP047-2","GRP048-2", // timeout
                     //"GRP123-1.003","GRP123-2.003", // ok
                     //"GRP123-3.003","GRP123-4.003","GRP123-6.003","GRP123-7.003","GRP123-8.003","GRP123-9.003", // ok
                     //"GRP124-1.004", // timeout
                     //"GRP124-2.004","GRP124-3.004","GRP124-4.004","GRP124-6.004","GRP124-7.004", // timeout
                     //"GRP124-8.004","GRP124-9.004", // timeout
                     //"GRP125-1.003", // ok
                     //"GRP125-2.005","GRP125-3.005", // timeout
                     //"GRP125-4.003", // ok
                     //"GRP126-1.004", // timeout
                     //"GRP126-2.004", // ok
                     //"GRP126-3.004","GRP126-4.004", // timeout
                     //"GRP127-1.004","GRP127-2.006","GRP127-3.004","GRP127-4.004", // timeout
                     //"GRP128-1.003", // timeout
                     //"GRP128-2.006","GRP128-3.005", // timeout
                     //"GRP128-4.003", // timeout
                     //"GRP129-1.003", // timeout
                     //"GRP129-2.004","GRP129-3.004", // timeout
                     //"GRP129-4.004", // timeout
                     //"GRP130-1.003", // timeout
                     //"GRP130-2.003", // timeout
                     //"GRP130-3.003", // timeout
                     //"GRP130-4.003", // timeout
                     //"GRP131-1.002", // timeout
                     //"GRP131-2.002", // timeout
                     //"GRP132-1.002", // timeout
                     //"GRP132-2.002", // timeout
                     //"GRP133-1.003", // timeout
                     //"GRP133-2.003", // timeout
                     //"GRP134-1.003", // timeout
                     //"GRP134-2.003", // timeout
                     //"GRP135-1.002", // timeout
                     //"GRP135-2.002", // ok
                     //"HWV003-3", // timeout
                     // "HWV005-2","HWV008-2.001", // ok
                     //"KRS002-1","KRS004-1","KRS010-1","KRS013-1","KRS017-1", // ok
                     //"LAT260-2","LAT261-2","LAT264-2","LAT265-2","LAT267-2","LAT270-2","LAT272-2","LAT273-2", // ok
                     //"LCL002-1","LCL003-1","LCL004-1","LCL005-1","LCL006-1","LCL007-1","LCL008-1","LCL009-1","LCL010-1","LCL011-1","LCL012-1","LCL013-1","LCL014-1","LCL015-1","LCL016-1","LCL017-1","LCL018-1","LCL019-1","LCL021-1","LCL022-1","LCL023-1","LCL024-1","LCL025-1","LCL026-1","LCL027-1","LCL029-1","LCL030-1","LCL033-1","LCL034-1","LCL035-1","LCL036-1","LCL040-1","LCL041-1","LCL042-1","LCL043-1","LCL044-1","LCL045-1","LCL046-1","LCL047-1","LCL048-1","LCL049-1","LCL050-1","LCL051-1","LCL052-1","LCL053-1","LCL054-1","LCL055-1","LCL056-1","LCL057-1","LCL058-1","LCL059-1","LCL060-1","LCL064-1","LCL064-2","LCL065-1","LCL066-1","LCL067-1","LCL068-1","LCL069-1","LCL070-1","LCL071-1","LCL072-1","LCL075-1","LCL076-1","LCL076-2","LCL076-3","LCL077-1","LCL077-2","LCL079-1","LCL080-1","LCL080-2","LCL081-1","LCL082-1","LCL083-1","LCL083-2","LCL084-3","LCL085-1","LCL086-1","LCL087-1","LCL088-1","LCL089-1","LCL090-1","LCL091-1","LCL093-1","LCL094-1","LCL095-1","LCL096-1","LCL097-1","LCL098-1","LCL100-1","LCL101-1",
                     //"LCL102-1","LCL103-1","LCL104-1","LCL106-1","LCL107-1","LCL108-1","LCL110-1","LCL111-1","LCL112-1","LCL113-1","LCL114-1","LCL115-1","LCL116-1","LCL117-1","LCL118-1","LCL120-1","LCL121-1","LCL122-1","LCL123-1","LCL126-1","LCL127-1","LCL128-1","LCL129-1","LCL130-1","LCL131-1","LCL166-1","LCL169-1","LCL170-1","LCL171-1","LCL172-1","LCL173-1","LCL174-1","LCL175-1","LCL176-1","LCL177-1","LCL178-1","LCL181-2","LCL182-1","LCL185-1","LCL186-1","LCL187-1","LCL188-1","LCL189-1","LCL190-1","LCL191-1","LCL192-1","LCL193-1","LCL194-1","LCL195-1","LCL196-1","LCL197-1","LCL198-1","LCL199-1","LCL200-1","LCL201-1","LCL202-1","LCL203-1","LCL204-1","LCL205-1","LCL206-1","LCL207-1","LCL208-1","LCL211-1","LCL212-1","LCL213-1","LCL215-1","LCL216-1","LCL221-1","LCL223-1","LCL224-1","LCL225-1","LCL226-1","LCL230-2","LCL234-1","LCL236-1","LCL237-1","LCL238-1","LCL250-1","LCL256-1","LCL257-1","LCL355-1","LCL356-1","LCL357-1","LCL358-1","LCL360-1","LCL361-1","LCL362-1","LCL363-1","LCL364-1","LCL366-1","LCL367-1",
                     //"LCL370-1","LCL371-1","LCL373-1","LCL378-1","LCL380-1","LCL381-1","LCL382-1","LCL384-1","LCL385-1","LCL386-1","LCL387-1","LCL390-1","LCL391-1","LCL396-1","LCL397-1","LCL398-1","LCL399-1","LCL400-1","LCL401-1","LCL402-1","LCL403-1","LCL404-1","LCL405-1","LCL414-1","LCL416-1","LCL429-2","LCL430-2","LCL432-2","LCL433-2","LCL435-2","LCL436-2","LCL437-2","LCL438-2","LCL439-2","LCL440-2","LCL441-2","LCL443-2","LCL445-2","LCL446-2","LCL447-2",
                     //"MGT001-1","MGT003-1","MGT004-1","MGT006-1","MGT008-1","MGT009-1","MGT010-1","MGT015-1", // ok
                     //"MGT018-1","MGT022-1","MGT022-2","MGT030-1","MGT032-2","MGT036-2","MGT036-3","MGT036-1","MGT041-2", // ok
                     //"MSC005-1","MSC006-1", // ok
                     //"MSC007-1.008", // infinite loop (timeout?)
                     //"MSC008-1.002","MSC008-2.002", // timeout
                     //"MSC015-1.005", // ok
                     //"MSC015-1.010","MSC015-1.015", // timeout
                     //"NLP001-1","NLP117-1","NLP122-1", // ok
                     //"NUM001-1","NUM002-1","NUM003-1","NUM004-1","NUM014-1","NUM015-1","NUM016-2","NUM024-1","NUM025-2","NUM283-1.005",
                     //"PLA001-1","PLA002-1","PLA003-1","PLA006-1", // ok
                     //"PLA007-1","PLA016-1", // timeout
                     //"PLA017-1","PLA019-1","PLA020-1", // ok
                     //"PLA022-1","PLA022-2", // timeout
                     //"PLA031-1.001", // ok
                     //"PLA031-1.002", // timeout
                     //"PUZ001-1","PUZ002-1","PUZ003-1","PUZ004-1", // ok
                     //"PUZ005-1", // timeout
                     //"PUZ008-1", // ok
                     //"PUZ008-3", // timeout
                     //"PUZ009-1", // ok
                     //"PUZ010-1", // timeout
                     //"PUZ011-1","PUZ012-1","PUZ013-1","PUZ014-1", // ok
                     //"PUZ015-2.006","PUZ016-2.005", // timeout
                     //"PUZ017-1", // timeout
                     //"PUZ018-1","PUZ019-1","PUZ021-1","PUZ022-1", // timeout
                     //"PUZ023-1","PUZ024-1","PUZ025-1", // ok
                     //"PUZ026-1", // timeout
                     //"PUZ027-1", // ok
                     //"PUZ028-5","PUZ028-6", // timeout
                     //"PUZ029-1", // ok
                     //"PUZ030-1", // timeout
                     //"PUZ030-2", // infinite loop (timeout?)
                     //"PUZ032-1", // timeout
                     //"PUZ033-1", // ok
                     //"PUZ035-1","PUZ035-2","PUZ035-4","PUZ035-7", // timeout
                     //"PUZ038-1", // ok
                     //"PUZ039-1","PUZ040-1","PUZ042-1", // timeout
                     //"PUZ047-1", // ok
                     //"PUZ056-2.005", // timeout
                     //"RNG001-2", // timeout
                     //"RNG001-3", // ok
                     //"RNG001-5", // timeout
                     //"RNG005-2","RNG006-2","RNG037-2","RNG038-2", // ok
                     //"RNG039-2" // timeout
                     //"SET001-1", // ok
                     //"SET002-1", // timeout
                     //"SET003-1","SET004-1", // ok
                     //"SET014-2", // timeout
                     //"SET043-5", // ok
                     //"SET045-5","SET047-5", // timeout
                     //"SET786-1","SET818-2","SET819-2","SET824-2","SET826-2","SET827-2","SET828-2","SET829-2", // ok
                     //"SET832-2","SET833-2","SET835-2","SET836-2","SET846-2","SET850-2","SET852-2","SET856-2","SET859-2", // ok
                     //"SWV001-1","SWV009-1","SWV011-1","SWV239-2","SWV242-2","SWV256-2","SWV257-2","SWV258-2","SWV259-2","SWV263-2","SWV264-2","SWV265-2","SWV266-2","SWV267-2","SWV269-2","SWV270-2","SWV271-2","SWV272-2","SWV273-2","SWV277-2","SWV279-2","SWV280-2","SWV291-2","SWV299-2","SWV309-2","SWV310-2","SWV312-2","SWV313-2","SWV314-2","SWV315-2","SWV320-2","SWV328-2","SWV330-2","SWV331-2","SWV333-2","SWV334-2","SWV335-2","SWV336-2","SWV341-2","SWV349-2","SWV350-2","SWV351-2","SWV355-2","SWV356-2","SWV357-2","SWV358-2","SWV364-2","SWV419-1.005",
                     //"SYN001-1.005", // timeout
                     //"SYN002-1.007.008", // ok
                     //"SYN003-1.006","SYN004-1.007","SYN005-1.010","SYN006-1","SYN008-1","SYN009-1","SYN009-2", // ok
                     //"SYN009-3","SYN009-4","SYN010-1.005.005","SYN011-1","SYN012-1","SYN014-2","SYN015-2", // ok
                     //"SYN028-1","SYN029-1","SYN030-1","SYN031-1","SYN032-1","SYN033-1","SYN034-1","SYN035-1", // ok
                     //"SYN038-1","SYN040-1","SYN041-1","SYN044-1","SYN045-1","SYN046-1","SYN047-1","SYN048-1", // ok
                     //"SYN049-1","SYN050-1","SYN051-1","SYN052-1","SYN053-1","SYN054-1","SYN055-1","SYN057-1", // ok
                     //"SYN058-1","SYN060-1","SYN061-1","SYN062-1","SYN063-1","SYN063-2","SYN064-1","SYN065-1", // ok
                     //"SYN068-1","SYN069-1","SYN070-1","SYN073-1","SYN079-1","SYN081-1","SYN084-2","SYN088-1.010", // ok
                     //"SYN095-1.002","SYN096-1.008","SYN099-1.003","SYN100-1.005","SYN101-1.002.002", // ok
                     //"SYN102-1.007.007", // timeout
                     //"SYN103-1","SYN104-1","SYN105-1","SYN106-1","SYN107-1","SYN108-1", // ok
                     //"SYN109-1","SYN110-1", // timeout
                     //"SYN111-1","SYN112-1","SYN113-1","SYN114-1", // ok
                     //"SYN115-1", // timeout
                     //"SYN116-1", // ok
                     //"SYN117-1", // timeout
                     //"SYN118-1","SYN119-1","SYN120-1","SYN121-1","SYN122-1","SYN123-1","SYN124-1","SYN125-1", // ok
                     //"SYN126-1","SYN127-1", // ok
                     //"SYN128-1","SYN129-1", // timeout
                     //"SYN130-1","SYN131-1","SYN132-1","SYN133-1","SYN134-1","SYN135-1","SYN136-1", // ok
                     //"SYN137-1", // timeout
                     //"SYN138-1","SYN139-1","SYN140-1","SYN141-1","SYN142-1","SYN143-1","SYN144-1","SYN145-1","SYN146-1","SYN147-1","SYN148-1","SYN149-1","SYN150-1","SYN151-1","SYN152-1","SYN153-1","SYN154-1","SYN155-1","SYN156-1","SYN157-1","SYN158-1","SYN159-1","SYN160-1","SYN161-1","SYN162-1","SYN163-1","SYN164-1","SYN165-1","SYN166-1","SYN167-1","SYN168-1","SYN169-1","SYN170-1","SYN171-1","SYN172-1","SYN173-1","SYN174-1","SYN175-1","SYN176-1","SYN177-1","SYN178-1","SYN179-1","SYN180-1","SYN181-1","SYN182-1","SYN183-1","SYN184-1","SYN185-1","SYN186-1","SYN187-1","SYN188-1","SYN189-1","SYN190-1","SYN191-1","SYN192-1","SYN193-1","SYN194-1","SYN195-1","SYN196-1","SYN197-1","SYN198-1","SYN199-1","SYN200-1","SYN201-1","SYN202-1","SYN203-1","SYN204-1","SYN205-1","SYN206-1","SYN207-1","SYN208-1","SYN209-1","SYN210-1","SYN211-1","SYN212-1","SYN213-1","SYN214-1","SYN215-1","SYN216-1","SYN217-1","SYN218-1","SYN219-1","SYN220-1","SYN221-1","SYN222-1","SYN223-1","SYN224-1","SYN225-1","SYN226-1","SYN227-1",
                     //"SYN228-1","SYN229-1","SYN230-1","SYN231-1","SYN232-1","SYN233-1","SYN234-1","SYN235-1","SYN236-1","SYN237-1","SYN238-1","SYN239-1","SYN240-1","SYN241-1","SYN242-1","SYN243-1","SYN244-1","SYN245-1","SYN246-1","SYN247-1","SYN248-1","SYN249-1","SYN250-1","SYN251-1","SYN252-1","SYN253-1","SYN254-1","SYN255-1","SYN256-1","SYN257-1","SYN258-1","SYN259-1","SYN260-1","SYN261-1","SYN262-1","SYN263-1","SYN264-1","SYN265-1","SYN266-1","SYN267-1","SYN268-1","SYN269-1","SYN270-1","SYN271-1","SYN272-1","SYN273-1","SYN274-1","SYN275-1","SYN276-1","SYN277-1","SYN278-1","SYN279-1","SYN280-1","SYN281-1","SYN282-1","SYN283-1","SYN284-1","SYN285-1","SYN286-1","SYN287-1","SYN288-1","SYN289-1","SYN290-1","SYN291-1","SYN292-1","SYN293-1","SYN294-1","SYN295-1","SYN296-1","SYN297-1","SYN298-1","SYN299-1","SYN300-1","SYN301-1","SYN310-1","SYN312-1","SYN315-1","SYN318-1","SYN321-1","SYN323-1","SYN326-1","SYN331-1","SYN332-1","SYN336-1","SYN338-1","SYN339-1","SYN340-1","SYN341-1","SYN343-1","SYN345-1",
                     //"SYN346-1","SYN347-1","SYN349-1","SYN350-1","SYN553-1","SYN554-1","SYN555-1","SYN558-1","SYN559-1","SYN561-1","SYN562-1","SYN564-1","SYN565-1","SYN566-1","SYN567-1","SYN568-1","SYN569-1","SYN570-1","SYN572-1","SYN574-1","SYN577-1","SYN580-1","SYN581-1","SYN582-1","SYN583-1","SYN584-1","SYN585-1","SYN587-1","SYN590-1","SYN593-1","SYN594-1","SYN596-1","SYN597-1","SYN602-1","SYN603-1","SYN605-1","SYN613-1","SYN618-1","SYN620-1","SYN621-1","SYN624-1","SYN626-1","SYN627-1","SYN628-1","SYN629-1","SYN630-1","SYN632-1","SYN651-1","SYN652-1","SYN659-1","SYN663-1","SYN666-1","SYN667-1","SYN668-1","SYN669-1","SYN670-1","SYN671-1","SYN672-1","SYN674-1","SYN675-1","SYN676-1","SYN677-1","SYN678-1","SYN680-1","SYN681-1","SYN682-1","SYN683-1","SYN684-1","SYN685-1","SYN686-1","SYN691-1","SYN692-1","SYN693-1","SYN698-1","SYN710-1","SYN714-1","SYN715-1","SYN719-1","SYN721-1","SYN724-1","SYN726-1","SYN727-1","SYN729-1","SYN731-1","SYN915-1",
                     //"TOP001-2","TOP002-2","TOP004-2","TOP005-2" // ok
//                     )
  val problems = Seq("ANA013-2","ANA025-2","ANA037-2","ANA038-2","ANA039-2","ANA041-2","ANA042-2","CAT007-3","COL101-2","COL102-2","COL104-2","COL105-2","COL109-2","COL110-2","COL111-2")

  val problemsGen = Gen.enumeration("gen")(problems :_*)


  performance of "Scavenger" in {
    measure method "EasyCNFUnsatNoEq" config (exec.benchRuns -> 3) in {
      using(problemsGen) in { problem =>
        println()
        println("!!!!!!!!!! " +  problem + "!!!!!!!!!!!")
        println()
        test(problem)
      }
    }

  }
}