package advisobot.core
import scala.language.implicitConversions
import scala.collection.{Map,SortedMap} // {Iterable,Map,Set,Seq}
import scala.collection.mutable.{HashSet,HashMap,ListBuffer}
import java.nio.file.{Path, Paths, Files, FileSystems}
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}
import org.maraist.util.UniqueHashCode
import org.maraist.outlines.{Outline}

import advisobot.core.Trace._
import advisobot.core.Strings._

trait PersonReport {
  def writeReport(doc: LaTeXdoc, who: Person)(implicit advisees: Advisees): Unit
}

class DefaultPersonReport()
extends PersonReport {
  def writeReport(doc: LaTeXdoc, who: Person)(implicit advisees: Advisees): Unit = {
    val forTerm = advisees.forTerm
    val lastPast = advisees.lastPast

    // Calculate a recommended schedule if appropriate.
    val recommended = who.recommend.get(forTerm) match {
      case Some(_) => who.recommend
      case None => who.calculateRecommendationIfEmpty && who.active match {
        case false => who.recommend
        case true => who.planSched(forTerm)
      }
    }

    val disclaimerSuffix = """    \vspace*{\fill}
    \par
    {\small \textsl{\textbf{Notice}:  This report is not an official
    ${advisees.institutionShortName} product, and is not automatically
    synchronized with the ${advisees.registrarName} databases.
    GPA calculations are approximate.
    Consult ${advisees.recordsSystemName} for official status
    information.}}""";

    var nowOrForward = SortedMap[Term, List[ScheduleSuggestion]]()
    for ((semester, plan) <- recommended) {
      if (lastPast < semester) {
        nowOrForward = nowOrForward ++ SortedMap(semester -> plan)
      }
    }

    traceBegin("writeHandout", "%s %s", who.firstNames, who.lastName)
    doc.classOptions = Some("11pt")
    doc.addPackage("inputenc","utf8")
    doc.addPackage("fontenc","T1")
    doc.addPackage("fixltx2e")
    doc.addPackage("graphicx")
    doc.addPackage("grffile")
    doc.addPackage("longtable")
    doc.addPackage("wrapfig")
    doc.addPackage("rotating")
    doc.addPackage("ulem","normalem")
    doc.addPackage("amsmath")
    doc.addPackage("textcomp")
    doc.addPackage("amssymb")
    doc.addPackage("capt-of")
    doc.addPackage("hyperref")
    doc.addPackage("array")
    doc.addPackage("fancyhdr")
    doc.addPackage("xcolor","table")
    doc.addPackage("makecell,tikz,latexsym,pifont,pdflscape,multicol,paralist")
    doc.addPackage("geometry","left=0.5in,right=0.5in,top=0.75in,bottom=0.75in")
    doc.addPreamble("\\usetikzlibrary{matrix,arrows.meta,calc}")
    doc.addPreamble("""\pgfdeclarelayer{background}""")
    doc.addPreamble("""\pgfdeclarelayer{foreground}""")
    doc.addPreamble("""\pgfsetlayers{background,main,foreground}""")
    doc.addPreamble("\\newcommand{\\classcheckshort}{\\underline{\\hspace*{10mm}}}")
    doc.addPreamble("""\newcommand{\checkgap}{\rule{0mm}{7mm}}""")
    doc.addPreamble("""\newcommand{\writegap}{\rule{0mm}{15mm}}""")
    doc.addPreamble("""\newcommand{\classcheck}{\classcheckshort\checkgap}""")
    doc.addPreamble("""\date{}""")
    doc.addPreamble("""\title{Course planner}""")
    doc.addPreamble("""\hypersetup{
 pdfauthor={John Maraist},
 pdftitle={Course planner},
 pdfkeywords={},
 pdfsubject={},
 pdfcreator={Emacs 24.5.1 (Org mode 8.3.3)},
 pdflang={English}}

\pagestyle{fancy}
\renewcommand\headrule{}
\cfoot{\parbox{\textwidth}{\small \textsl{\textbf{Notice}:  This report is not an official
    UWL product, and is not automatically synchronized with the Records
    \& Registration databases.  GPA calculations are approximate.
    Consult WINGS for official status information.}}}""")

    doc.open()
    doc ++= """
\parindent 0pt
\newcommand{\Semester}{""" + forTerm + """}
\newcommand{\reqBoxVspace}{\vspace{1.5mm}}

\newcommand{\oneNode}[2]{\begin{tikzpicture}[align=center]\node[#1]{#2};\end{tikzpicture}}
\newcommand{\circTwo}{\ding{193}}
\newcommand{\geCheck}{\node[fill=orange!30]{\checkmark};}
\newcommand{\pgCheck}{\node[fill=yellow]{\checkmark};}
\newcommand{\bsmsCheck}{\node[fill=blue!15]{\checkmark};}
\newcommand{\bsmsFive}{\node[fill=blue!15]{\ding{196}};}
\newcommand{""" + ugCheck + """}{\node[fill=green!15]{\checkmark};}
\newcommand{\reqseg}{\draw[very thick, color=red]}
\newcommand{\prereq}{\draw[very thick, color=red, ->, >=Triangle]}
\newcommand{\altprereq}{\draw[dotted, color=red, ->, >=Triangle]}
\newcommand{\NO}{\node{\small\textsc{no}};}
    \setlength{\columnsep}{1mm}
    \begin{center}
      \huge\textbf{CS major course planner --- \Semester}
    \end{center}
    \begin{tabular}[t]{|l|l|}
      \hline\multicolumn{2}{|l|}{Name}
      \\ \multicolumn{2}{|c|}{\textbf{"""
      doc ++= who.firstNames
      doc ++= " "
      doc ++= who.lastName
      doc ++= "}}\n  \\\\ \\multicolumn{2}{|l|}{Program}\n";
    for (prog <- who.programs) {
      if (prog.viewAsProgram) {
        val name = prog.name
        doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\textbf{"
        doc ++= name.replaceAll("\\. ", ".\\ ").replaceAll("&", "\\&")
        doc ++= "}}\n"
      }
    }

    val pastUnits = who.unitsCompleted
    val currentUnits = who.current.foldLeft(0)(_+_.units)
    val totalUnits = who.unitsProspective
    if (totalUnits > 0) {
      doc ++= "  \\\\ \\multicolumn{2}{|l|}{Units}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Previously --- "
      doc ++= pastUnits.toString()
      doc ++= "}}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Currently --- "
      doc ++= currentUnits.toString()
      doc ++= "}}\n"
      doc ++= "  \\\\ \\multicolumn{2}{|c|}{\\large \\textbf{Total --- "
      doc ++= totalUnits.toString()
      doc ++= "}}\n"
    } else {
      doc ++= """  \\ \multicolumn{2}{|l|}{Units earned}
      \\ \multicolumn{2}{|c|}{\writegap}
"""
    }

    val filesys = FileSystems.getDefault()
    val photosDir: Path = filesys.getPath(advisees.photoDirectory);
    if (Files.isDirectory(filesys.getPath(advisees.reportDirectory + "/" + photosDir.toString()))) {
      var photoFile = advisees.photoDirectory + "/" + who.id + ".jpg"
      if (!(new java.io.File(photoFile).exists)) {
        photoFile = advisees.photoDirectory + "/" + who.id + ".jpeg"
      }
      if (!(new java.io.File(photoFile).exists)) {
        photoFile = advisees.photoDirectory + "/" + who.id + ".png"
      }
      if ( new java.io.File(photoFile).exists) {
        // println("Using ", photoFile)
        doc ++= "  \\\\ \\multicolumn{2}{|c|}{{\\includegraphics{"
        advisees.reportToPhotoDirPath match {
          case None => { }
          case Some(p) => {
              doc ++= p
            doc ++= "/"
          }
        }
        doc ++= photoFile
        doc ++= "}}}\n"
      } else {
        // println("No photo file ", photoFile)
      }
    } else {
      DefaultPersonReport.warnNoImageDirectory(advisees.photoDirectory)
    }
    // doc ++= "\\vspace*{-15mm}\n";

    doc ++= """      \\ \hline\multicolumn{2}{c}{\rule{0mm}{5mm}Course plan for """
    doc ++= forTerm.toString()
    doc ++= "}\n      \\\\ \\hline Units & Class\n"

    recommended.get(forTerm) match {
      case Some(recommendations) => {
        val oldFormatter = SelectionFormatter.currentFormatter
        SelectionFormatter.currentFormatter =
          SelectionFormatter.handwrittenFormatter
        val selectionFormatter = SelectionFormatter.currentFormatter
        var totalUnits = recommendations.length match {
          case 0 => NoLoadInfo.NO_INFO
          case _ => UnitsRange.exactly(0)
        }
        for(rec <- recommendations) {
          rec.toLaTeX(doc)
          totalUnits = totalUnits and rec.units
        }
        doc ++= "      \\\\ \\hline\n"
        totalUnits match {
          case _: NoLoadInfo => { }
          case tu: UnitsRange => {
            doc ++= """      \multicolumn{2}{c}{\textcolor{"""
            doc ++= selectionFormatter.colorName
            doc ++= "}{"
            doc ++= selectionFormatter.formatter
            tu.toLaTeX(doc)
            doc ++= " total units}}\n"
          }
        }

        if (nowOrForward.size > 1) {
          doc ++= """\\[1em]\multicolumn{2}{c}{\textcolor{"""
          doc ++= selectionFormatter.colorName
          doc ++= "}{"
          doc ++= selectionFormatter.formatter
          doc ++= "Plans for further}}"

          doc ++= """\\\multicolumn{2}{c}{\textcolor{"""
          doc ++= selectionFormatter.colorName
          doc ++= "}{"
          doc ++= selectionFormatter.formatter
          doc ++= " semesters on}}"

          doc ++= """\\\multicolumn{2}{c}{\textcolor{"""
          doc ++= selectionFormatter.colorName
          doc ++= "}{"
          doc ++= selectionFormatter.formatter
          doc ++= " the next page}}"
        }
        SelectionFormatter.currentFormatter = oldFormatter
      }
      case None => {
        doc ++= """      \\ \writegap & \rule{3cm}{0mm}
      \\ \hline \writegap &
      \\ \hline \writegap &
      \\ \hline \writegap &
      \\ \hline \writegap &
      \\ \hline \writegap &
      \\ \hline \writegap &
      \\ \hline
"""
      }
    }

    doc ++= """    \end{tabular}
    %%
    %%
    \begin{tabular}[t]{l}
"""

    // The main part of the first page --- the for loop shows all of
    // the advising boxes.  Then we add in the outline of notes for
    // this semester (if any).
    var sep:String = ""
    for (prog <- who.programs) {
      doc ++= sep
      prog.writeForHandout(doc, who)
    }
    {
      doc ++= s"\\begin{minipage}{${who.notesWidth}}\n\\textcolor{"
      doc ++= SelectionFormatter.handwrittenFormatter.colorName
      doc ++= "}{"
      doc ++= SelectionFormatter.handwrittenFormatter.formatter

      who.notes.get(forTerm) match {
        case Some(n) => {
          val thisShrinkNotes: Int = {
            var res: Int = 0
            if (advisees.shrinkNotes < 0) {
              res = 0
            } else if (advisees.shrinkNotes == 0) {
              res = who.shrinkNotes
            } else if (who.shrinkNotes < 0) {
              res = 0
            } else if (who.shrinkNotes > advisees.shrinkNotes) {
              res = who.shrinkNotes
            } else {
              res = advisees.shrinkNotes
            }
            res
          }

          if (thisShrinkNotes > 4) {
            doc ++= "\\fontsize{9}{10}\\selectfont "
          } else if (thisShrinkNotes > 3) {
            doc ++= "\\fontsize{9.5}{11}\\selectfont "
          } else if (thisShrinkNotes > 2) {
            doc ++= "\\fontsize{10}{11}\\selectfont "
          } else if (thisShrinkNotes > 1) {
            doc ++= "\\fontsize{11}{12}\\selectfont "
          } else if (thisShrinkNotes > 0) {
            doc ++= "\\fontsize{12}{13}\\selectfont "
          }

          n.toLaTeX(doc)
        }
        case None => { }
      }

      doc ++= "}\n\\end{minipage}\n"
    }

    doc ++= "    \\end{tabular}\n"
    doc ++= "\n    \\clearpage\n\n"

    if (who.past.size > 0 || who.current.size > 0) {
      val mostRecent = who.mostRecentlyTaken(lastPast)

      doc ++= "\\section*{Course history}\n"
      doc ++= "  \\begin{multicols}{3}\\raggedcolumns\\small\\setlength\\columnsep{1em}\n"
      doc ++= "    \\newlength{\\codewidth}\n"
      doc ++= "    \\newlength{\\unitswidth}\n"
      doc ++= "    \\newlength{\\gradewidth}\n"
      doc ++= "    \\newlength{\\spacewidth}\n"
      doc ++= "    \\newlength{\\namewidth}\n"
      doc ++= "    \\newlength{\\fullwidth}\n"
      doc ++= "    \\newlength{\\nameonlywidth}\n"
      doc ++= "    \\settowidth{\\codewidth}{MTH\\,388}\n"
      doc ++= "    \\settowidth{\\unitswidth}{88}\n"
      doc ++= "    \\settowidth{\\gradewidth}{TC+}\n"
      doc ++= "    \\settowidth{\\spacewidth}{~}\n"
      doc ++= "    \\setlength{\\namewidth}{0.16\\textwidth}\n"
      doc ++= "    \\settowidth{\\nameonlywidth}{~}\n"
      doc ++= "    \\addtolength{\\nameonlywidth}{\\namewidth}\n"
      doc ++= "    \\addtolength{\\nameonlywidth}{\\gradewidth}\n"
      doc ++= "    \\settowidth{\\fullwidth}{~~~}\n"
      doc ++= "    \\addtolength{\\fullwidth}{\\codewidth}\n"
      doc ++= "    \\addtolength{\\fullwidth}{\\nameonlywidth}\n"
      doc ++= "    \\addtolength{\\fullwidth}{\\unitswidth}\n"

      for (semester <- who.past.keys) {
        var totalUnits: Int = 0
        doc ++= "\\begin{tabular}{|@{\\,}p{\\codewidth}@{~}p{\\namewidth}@{~~}p{\\unitswidth}@{~}p{\\gradewidth}@{\\,}|}\n"
        doc ++= "\\multicolumn{4}{@{}l@{}}{\\textbf{"
        doc ++= semester.toString()
        doc ++= "}} \\\\ \\hline\n"

        for ((course, grade) <- who.past(semester)) {
          val precolor = mostRecent(course).equals(semester) match {
            case true => ""
            case false => "\\textcolor{gray}{"
          }
          val postcolor = mostRecent(course).equals(semester) match {
            case true => ""
            case false => "}"
          }

          totalUnits = totalUnits + course.units
          doc ++= " \\raggedright "
          doc ++= precolor
          course.formatSatisfier(doc)
          doc ++= postcolor
          doc ++= " & \\raggedright "
          doc ++= precolor
          doc ++=* furtherShorten(course.shortName)
          doc ++= postcolor
          doc ++= " & "
          doc ++= precolor
          doc ++= course.units.toString()
          doc ++= postcolor
          doc ++= " & "
          doc ++= precolor
          doc ++= grade.toString()
          doc ++= postcolor
          doc ++= " \\\\\n"
        }
        doc ++= s"\\hline\\multicolumn{3}{r@{~~}}{Total registered units: $totalUnits}\\end{tabular}\n\\\\[2pt]\n"
      }

      if (who.current.size > 0) {
        doc ++= "\\begin{tabular}{|@{\\,}p{\\codewidth}@{~}p{\\nameonlywidth}@{~~}p{\\unitswidth}@{\\,}|}\n"
        doc ++= "\\multicolumn{2}{@{}l@{}}{\\textbf{Current enrollment}} \\\\ \\hline\n"

        var totalUnits = 0
        for ((course) <- who.current) {
          doc ++= " \\raggedright "
          course.formatSatisfier(doc)
          doc ++= " & \\raggedright "
          doc ++=* furtherShorten(course.shortName)
          doc ++= " & "
          totalUnits = totalUnits + course.units
          doc ++= course.units.toString()
          doc ++= " \\\\\n"
        }
        doc ++= s"\\hline\\multicolumn{3}{@{}c@{}}{Total semester units: $totalUnits}\n"

        if (recommended.contains(lastPast)) {
          doc ++= "\\\\[2pt]\\multicolumn{3}{@{\\,}p{\\fullwidth}@{\\,}}{Advisor had recommended for current enrollment: \\raggedright "
          var recSep = ""
          for (rec <- recommended(lastPast)) {
            doc ++= recSep
            doc ++= rec.description.plainText
            recSep = "; "
          }
          doc ++= ".}\n"
        }

        doc ++= "\\end{tabular}\n\\\\[2pt]\n"
      }

      doc ++= "    \\end{multicols}\n"
    }

    who.gpa() match {
      case None => { }
      case Some(_) => {
        doc ++= "\\begin{center}\n"
        gpaByTermGraph(doc, who, { x => who.gpa(term = Some(x)) },
                       title = "Semester GPA")
        doc ++= "\\hspace{2em}\n"
        gpaByTermGraph(doc, who, { x => who.gpa(cumulative = true,
                                                term = Some(x)) },
                       title = "Cumulative GPA")
        doc ++= "\\hspace{2em}\n"
        gpaByTermGraph(doc, who, { x => who.gpa(prefix=Some("CS"),
                                                term=Some(x)) },
                       title = "Semester CS GPA")
        doc ++= "\\hspace{2em}\n"
        gpaByTermGraph(doc, who, { x => who.gpa(prefix=Some("CS"),
                                                cumulative=true,
                                                term=Some(x)) },
                       title = "Cumulative CS GPA")
        doc ++= "\\end{center}\n"
      }
    }

    // Section about future plans, if planned out more than one semester
    if (nowOrForward.size > 1) {
      doc ++= "\\section*{Forward plan}\n\\raggedright\n"
      doc ++= "\\begin{multicols}{4}\n"
      var postPlanUnits: UnitsRange = new UnitsRange(who.unitsProspective)

      for ((semester, plan) <- nowOrForward) {
        doc ++= """\begin{tabular}[t]{|c|l|} \multicolumn{2}{c}{\emph{"""
        semester.toLaTeX(doc)
        doc ++= "}}\n"

        var totalUnits = UnitsRange.exactly(0)
        for (item <- plan) {
          val hasPrereq = item.description.hasPrerequisiteIn(plan)
          if (hasPrereq) { ScheduleSuggestion.setAlertNext }
          item.toLaTeX(doc)
          doc ++= "\n"
          totalUnits = totalUnits and item.units
        }
        doc ++= """\\ \hline \multicolumn{2}{c}{"""
        totalUnits.toLaTeX(doc)
        postPlanUnits = postPlanUnits.and(totalUnits)
        doc ++= " total units}\n"
        doc ++= "\\end{tabular} \\hspace{1ex}\n"
      }

      doc ++= "\\end{multicols}\nTotal units after planned period: "
      postPlanUnits.toLaTeX(doc)
      doc ++= "\n"
    }

    traceEnd("writeHandout", "%s %s", who.firstNames, who.lastName)
  }

  def gpaByTermGraph(doc: LaTeXdoc, who: Person, calc: Term => Option[Double],
                     title: String = null) = {
    val semesterCount = who.past.size

    doc ++= "  \\begin{tikzpicture}[x=8mm,y=4mm]\n"
    doc ++= "    \\draw (-0.25,-0.5) rectangle ("
    doc ++= (semesterCount - 0.75).toString()
    doc ++= ",4.5);\n"
    title match {
      case null => { }
      case _ => {
        doc ++= "    \\node at ("
        doc ++= ((semesterCount-1)/2).toString()
        doc ++= ",5) {"
        doc ++= title
        doc ++= "};\n"
      }
    }
    doc ++= "    \\node[label distance=1mm,anchor=east] at (-0.25,0) {0.0};\n"
    doc ++= "    \\node[label distance=1mm,anchor=east] at (-0.25,1) {1.0};\n"
    doc ++= "    \\node[label distance=1mm,anchor=east] at (-0.25,2) {2.0};\n"
    doc ++= "    \\node[label distance=1mm,anchor=east] at (-0.25,3) {3.0};\n"
    doc ++= "    \\node[label distance=1mm,anchor=east] at (-0.25,4) {4.0};\n"
    var i=0
    for (semester <- who.past.keys) {
      doc ++= "    \\node[label={[text depth=-1ex,rotate=-45,font=\\small]right:"
      doc ++= semester.toString()
                      .replace(" 20", "\\,'")
                      .replace("Winter", "W")
                      .replace("Fall", "F")
                      .replace("Spring", "Spr")
                      .replace("Summer", "Sum")
      doc ++= "}] at ("
      doc ++= (i-0.25).toString()
      doc ++= ",-0.8) {};\n"
      i = i+1
    }

    var lastPoint: Option[String] = None
    i=0
    for (semester <- who.past.keys) {
      calc(semester) match {
        case Some(gpa) => {
          val name = "p" + i.toString()
          val color = gpa match {
            case x if x<2 => "red!75!black"
            case x if x<2.5 => "yellow!70!black"
            case _ => "green!63!black"
          }
          doc ++= "    \\node[circle,draw="
          doc ++= color
          doc ++= ", fill="
          doc ++= "white"
          doc ++= ", inner sep=0pt,minimum size=1.2mm] ("
          doc ++= name
          doc ++= ") at ("
          doc ++= i.toString()
          doc ++= ","
          doc ++= gpa.toString()
          doc ++= ") {};\n"

          lastPoint match {
            case None => { }
            case Some(point) => {
              doc ++= "    \\draw["
              doc ++= color
              doc ++= "] ("
              doc ++= point
              doc ++= ") to ("
              doc ++= name
              doc ++= ");\n"
            }
          }
          lastPoint = Some(name)
        }
        case None => {}
      }
      i = i+1
    }

    doc ++= "  \\draw[dotted,step=1] (0,0) grid ("
    doc ++= (semesterCount-1).toString()
    doc ++= ",4);\n"

    doc ++= "  \\end{tikzpicture}\n"
  }

  def furtherShorten(s: String): String =
    s.replace("Advanced ", "Adv. ")
     .replace("Elementary ", "Elem. ")
     .replace("Intermediate ", "Intermed. ")
     .replace("Communicating ", "Comm. ")
     .replace("Fundamental ", "Fund. ")
     .replace("economics", "econ.")
     .replace("Economics", "Econ.")
     .replace("French", "Fren.")
     .replace("Spanish", "Span.")
     .replace("Americana", "Amer.")
     .replace("Americans", "Amer.")
     .replace("American", "Amer.")
     .replace("Civilizations", "Civ.")
     .replace("Transfer Course", "Transfer")
     .replace("Civilization", "Civ.")
     .replace("Asm. Pr. & In. Cmp.", "Computer")
     .replace("Experiences", "Exp.")
     .replace("Experience", "Exp.")
     .replace(" in America (ES)", "")
     .replace(": ADTs", "")
     .replace("Global Transition ", "Gl. Trans. ")
     .replace("Philosophy of the ", "Phil. ")
     .replace("Sw. Design IV: ", "")
     .replace("Sw.Eng.", "Software Engineering")
}

object DefaultPersonReport {
  var warnedNoImageDirectory = false

  def warnNoImageDirectory(dir: String): Unit = {
    if (!warnedNoImageDirectory) {
      println("Warning: no image directory " + dir)
      warnedNoImageDirectory = true
    }
  }
}
