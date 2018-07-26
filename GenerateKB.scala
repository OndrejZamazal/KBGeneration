package cz.vse.swoe.benchmarks

import scala.collection.mutable
import scala.io.Source
import java.io._

object GenerateKB {
  
  val dataMajorActivity: mutable.Map[String, mutable.ArrayBuffer[String]] = mutable.Map.empty[String, mutable.ArrayBuffer[String]]
  val dataMinorActivity: mutable.Map[String, mutable.ArrayBuffer[String]] = mutable.Map.empty[String, mutable.ArrayBuffer[String]]
  
  def codeToAtt( code : String ) : String = {
    if(code.matches("[abc]")) "LanguageElementCombinations"
      else if(code.matches("[def]")) "EntityTypes"
        else if(code.matches("[ghi]")) "Size"
          else if(code.matches("[jkl]")) "LanguageComplexity"
            else "SWTechnology"
  }
  
  def prepareData( file : String) : Unit = {
    val lines = Source.fromFile(file).getLines().toList.drop(1)
    
    for (line <- lines) {
      val columns: Array[String] = line.split(",")
      val activity = columns(0).trim 
      //for(column <- columns.drop(1)) {
      for(i <- 1 until columns.size) {
        if(columns(i).matches(".*[()].*")) { //minor activity
          //println(column+" minor")
          val pole: mutable.ArrayBuffer[String] = dataMinorActivity.getOrElse(activity, mutable.ArrayBuffer.empty[String])
          val characteristics = columns(i).replaceAll("\\(", "").replaceAll("\\)", "").trim
          for(char <- characteristics) {
            //println(char)
            if(char.toString!="|")
              pole +=(char+"@B"+i)
          }
          dataMinorActivity+=(activity->pole)
        }
        else { //major actitivity
          val pole: mutable.ArrayBuffer[String] = dataMajorActivity.getOrElse(activity, mutable.ArrayBuffer.empty[String])
          val characteristics = columns(i).replaceAll("\\(", "").replaceAll("\\)", "").trim
          for(char <- characteristics) {
            //println(char)
            if(char.toString!="|")
              pole +=(char+"@B"+i)
          }
          dataMajorActivity+=(activity->pole)
        }
      }
    }
    println(dataMajorActivity)
    println(dataMinorActivity)
  }
  
  def generateKB() : Unit = {
    val numberOfBenchmarks = 21
    val pw = new PrintWriter(new File("kb.xml"))
    pw.println("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>")
    pw.println("<!DOCTYPE base SYSTEM \"base.dtd\">")
    pw.println("<base>")

    val allPossibleActivities: mutable.Set[String] = mutable.Set.empty[String]
    for(key <- dataMajorActivity.keys) {
      allPossibleActivities +=key
    }
    for(key <- (dataMinorActivity.keys)) {
      if(!dataMajorActivity.contains(key))
        allPossibleActivities +=key
    }
    
    //DONE global properties
    pw.println("<global><description>KB for recommending ontology benchmark.</description>")
    pw.println("<expert>Ondrej Zamazal</expert>")
    pw.println("<knowledge_engineer>Ondrej Zamazal</knowledge_engineer>")
    pw.println("<date>06/06/2018</date>")
    pw.println("<inference_mechanism>standard</inference_mechanism>")
    pw.println("<weight_range>1</weight_range>")
    pw.println("<default_weight>irrelevant</default_weight>")
    pw.println("<global_priority>first</global_priority>")
    pw.println("<context_global_threshold>0.000</context_global_threshold>")
    //pw.println("<condition_global_threshold>2.700</condition_global_threshold>")
    pw.println("<condition_global_threshold>0.800</condition_global_threshold>")
    pw.println("</global>")
    
    //DONE attributes and propositions
    pw.println("<attributes>")
    
    pw.println("<attribute>")
    pw.println("<id>Activity</id><name>Activity</name><type>multiple</type>")
    pw.println("<comment>Which activities of your tool you want to benchmark?</comment>")
    //pw.println("")
    pw.println("<propositions>")
    for(activity <- allPossibleActivities) {
      pw.println("<proposition><id>"+activity+"</id><name>"+activity+"</name></proposition>")
    }
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    //pw.println("<id>LanguageElementCombinations</id><name>Language Element Combinations</name><type>single</type>")
    pw.println("<id>LanguageElementCombinations</id><name>Language Element Combinations</name><type>multiple</type>")
    pw.println("<propositions>")
    pw.println("<proposition><id>a</id><name>specific</name></proposition>")
    pw.println("<proposition><id>b</id><name>most</name></proposition>")
    pw.println("<proposition><id>c</id><name>all</name></proposition>")
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    pw.println("<id>EntityTypes</id><name>Entity Types</name><type>multiple</type>")
    pw.println("<propositions>")
    pw.println("<proposition><id>d</id><name>TBox</name></proposition>")
    pw.println("<proposition><id>e</id><name>ABox</name></proposition>")
    pw.println("<proposition><id>f</id><name>specific</name></proposition>")
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    //pw.println("<id>Size</id><name>Size</name><type>single</type>")
    pw.println("<id>Size</id><name>Size</name><type>multiple</type>")
    pw.println("<propositions>")
    pw.println("<proposition><id>g</id><name>small</name></proposition>")
    pw.println("<proposition><id>h</id><name>moderate</name></proposition>")
    pw.println("<proposition><id>i</id><name>large</name></proposition>")
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    //pw.println("<id>LanguageComplexity</id><name>Language Complexity</name><type>single</type>")
    pw.println("<id>LanguageComplexity</id><name>Language Complexity</name><type>multiple</type>")
    pw.println("<propositions>")
    pw.println("<proposition><id>j</id><name>lightweight</name></proposition>")
    pw.println("<proposition><id>k</id><name>moderate</name></proposition>")
    pw.println("<proposition><id>l</id><name>very expressive</name></proposition>")
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    pw.println("<id>SWTechnology</id><name>SW Technology</name><type>multiple</type>")
    pw.println("<propositions>")
    pw.println("<proposition><id>m</id><name>RDF</name></proposition>")
    pw.println("<proposition><id>n</id><name>OWL</name></proposition>")
    pw.println("<proposition><id>o</id><name>SPARQL</name></proposition>")
    pw.println("</propositions>")
    pw.println("</attribute>")
    
    pw.println("<attribute>")
    pw.println("<id>Benchmark</id><name>Benchmark</name><type>multiple</type>")
    pw.println("<propositions>")
    for(i <- 1 to numberOfBenchmarks) {
      pw.println("<proposition><id>B"+i+"</id><name>B"+i+"</name></proposition>")  
    }
    pw.println("</propositions>")    
    pw.println("</attribute>")
    
		pw.println("</attributes>")
    
    //DONE contexts    
    //pw.println("")
		pw.println("<contexts>")
		for(activity <- allPossibleActivities) {
      pw.println("<context><id>CTX"+activity+"</id><condition><conjunction><literal><id_attribute>Activity</id_attribute><id_proposition>"+activity+"</id_proposition><negation>0</negation></literal></conjunction></condition></context>")
    }
		pw.println("</contexts>")
		
    //TODO compositional rules
		//pw.println("")
		pw.println("<rules>")
		pw.println("<apriori_rules></apriori_rules>")
		
		pw.println("<compositional_rules>")
		//pw.println("<compositional_rule>")
		
		//for negative rules
		val allPossibleAntecedentsForNegativeRules: Set[String] = Set("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
		//for tracking which benchmarks do not support which activities
		val benchmarksToActivities: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty[String, mutable.Set[String]]
		
		//compositional major rules
		for(activity <- dataMajorActivity.keys) {
		  val pole: mutable.ArrayBuffer[String] = dataMajorActivity.getOrElse(activity, mutable.ArrayBuffer.empty[String])
		  //val usedAntecedentsMajor: mutable.Set[String] = mutable.Set.empty[String]
		  //for negative rules
		  val usedAntecedentsMajor: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty[String, mutable.Set[String]]
		  
		  for(i <- 0 until pole.size) {
		    val cedents = pole(i).split("@")
		    val antecedent = cedents(0)
		    val succedent = cedents(1)
		    pw.println("<compositional_rule><id>"+activity+"MajorC"+(i+1)+"</id>"
		        +"<id_context>CTX"+activity+"</id_context>"
		        +"<condition><conjunction><literal><id_attribute>"+codeToAtt(antecedent)+"</id_attribute>"
		        +"<id_proposition>"+antecedent+"</id_proposition><negation>0</negation></literal></conjunction>"
		        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
		        +"<id_proposition>"+succedent+"</id_proposition><negation>0</negation>"
		        +"<weight>0.700</weight></conclusion></conclusions></compositional_rule>")
		    //for negative rules with context
		    val pole1: mutable.Set[String] = usedAntecedentsMajor.getOrElse(succedent, mutable.Set.empty[String])
		    pole1 += antecedent
		    usedAntecedentsMajor+=(succedent->pole1)
		    //for negative rules without context
		    val pole2: mutable.Set[String] = benchmarksToActivities.getOrElse(succedent, mutable.Set.empty[String])
		    pole2 += activity
		    benchmarksToActivities+=(succedent->pole2)
		  }
		  
		  //negative rules with context major
		  for(benchmark <- usedAntecedentsMajor.keys) {
		    var neg = 1
		    for(antecedent <- allPossibleAntecedentsForNegativeRules.diff(usedAntecedentsMajor.getOrElse(benchmark, mutable.Set.empty[String]))) {
		      pw.println("<compositional_rule><id>"+activity+"Neg"+benchmark+"MajorC"+neg+"</id>"
		        +"<id_context>CTX"+activity+"</id_context>"
		        +"<condition><conjunction><literal><id_attribute>"+codeToAtt(antecedent)+"</id_attribute>"
		        +"<id_proposition>"+antecedent+"</id_proposition><negation>0</negation></literal></conjunction>"
		        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
		        +"<id_proposition>"+benchmark+"</id_proposition><negation>1</negation>"
		        +"<weight>0.500</weight></conclusion></conclusions></compositional_rule>")
		      neg +=1
		    }
		  }
		  
    }
    
		
		//compositional minor rules
		
		for(activity <- dataMinorActivity.keys) {
		  val pole: mutable.ArrayBuffer[String] = dataMinorActivity.getOrElse(activity, mutable.ArrayBuffer.empty[String])
		  //for negative rules
		  val usedAntecedentsMinor: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty[String, mutable.Set[String]]
		  
		  for(i <- 0 until pole.size) {
		    val cedents = pole(i).split("@")
		    val antecedent = cedents(0)
		    val succedent = cedents(1)
		    pw.println("<compositional_rule><id>"+activity+"MinorC"+(i+1)+"</id>"
		        +"<id_context>CTX"+activity+"</id_context>"
		        +"<condition><conjunction><literal><id_attribute>"+codeToAtt(antecedent)+"</id_attribute>"
		        +"<id_proposition>"+antecedent+"</id_proposition><negation>0</negation></literal></conjunction>"
		        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
		        +"<id_proposition>"+succedent+"</id_proposition><negation>0</negation>"
		        +"<weight>0.300</weight></conclusion></conclusions></compositional_rule>")
		    //for negative rules
		    val pole1: mutable.Set[String] = usedAntecedentsMinor.getOrElse(succedent, mutable.Set.empty[String])
		    pole1 += antecedent
		    usedAntecedentsMinor+=(succedent->pole1)
		    //for negative rules without context
		    val pole2: mutable.Set[String] = benchmarksToActivities.getOrElse(succedent, mutable.Set.empty[String])
		    pole2 += activity
		    benchmarksToActivities+=(succedent->pole2)
		  }
		  
		  //negative rules with context minor
		  for(benchmark <- usedAntecedentsMinor.keys) {
		    var neg = 1
		    for(antecedent <- allPossibleAntecedentsForNegativeRules.diff(usedAntecedentsMinor.getOrElse(benchmark, mutable.Set.empty[String]))) {
		      pw.println("<compositional_rule><id>"+activity+"Neg"+benchmark+"MinorC"+neg+"</id>"
		        +"<id_context>CTX"+activity+"</id_context>"
		        +"<condition><conjunction><literal><id_attribute>"+codeToAtt(antecedent)+"</id_attribute>"
		        +"<id_proposition>"+antecedent+"</id_proposition><negation>0</negation></literal></conjunction>"
		        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
		        +"<id_proposition>"+benchmark+"</id_proposition><negation>1</negation>"
		        //+"<weight>0.500</weight></conclusion></conclusions></compositional_rule>")
		        +"<weight>0.250</weight></conclusion></conclusions></compositional_rule>")
		      neg +=1
		    }
		  }
		  
    }
    
		
		println(benchmarksToActivities)
		/* odebrano 08-06-18
		//compositional negative rules without context, i.e. for benchmarks which do not certain activity at all
		for(benchmark <- benchmarksToActivities.keys) {
	    var neg = 1
	    for(activity <- allPossibleActivities.diff(benchmarksToActivities.getOrElse(benchmark, mutable.Set.empty[String]))) {
	      pw.println("<compositional_rule><id>Neg"+benchmark+"C"+neg+"</id>"
	        +"<condition><conjunction><literal><id_attribute>Activity</id_attribute>"
	        +"<id_proposition>"+activity+"</id_proposition><negation>0</negation></literal></conjunction>"
	        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
	        +"<id_proposition>"+benchmark+"</id_proposition><negation>1</negation>"
	        +"<weight>0.700</weight></conclusion></conclusions></compositional_rule>")
	      neg +=1
	    }
	  }
		*/
		pw.println("</compositional_rules>")
		
		pw.println("<logical_rules>")
		//logical negative rules without context, i.e. for benchmarks which do not certain activity at all
		for(benchmark <- benchmarksToActivities.keys) {
	    var neg = 1
	    for(activity <- allPossibleActivities.diff(benchmarksToActivities.getOrElse(benchmark, mutable.Set.empty[String]))) {
	      pw.println("<logical_rule><id>Neg"+benchmark+"L"+neg+"</id>"
	        +"<condition_threshold>0.800</condition_threshold>"
	        +"<condition><conjunction><literal><id_attribute>Activity</id_attribute>"
	        +"<id_proposition>"+activity+"</id_proposition><negation>0</negation></literal></conjunction>"
	        +"</condition><conclusions><conclusion><id_attribute>Benchmark</id_attribute>"
	        +"<id_proposition>"+benchmark+"</id_proposition><negation>1</negation>"
	        +"</conclusion></conclusions></logical_rule>")
	      neg +=1
	    }
	  }
	  pw.println("</logical_rules>")
		
		pw.println("</rules>")
    
    pw.println("</base>")
    
    pw.close
  }
  
  def main(args: Array[String]) {
    prepareData("activityCharacteristicsTable.csv")
    generateKB()
  }
  
}
