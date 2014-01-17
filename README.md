[![License](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)](http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US)
![Logo](https://raw.github.com/Paradoxika/Skeptik/develop/res/logo150.png)

Skeptik is a collection of data structures and algorithms focused especially on the compression of formal proofs. 

Resolution proofs, in particular, are used by various sat-solvers, smt-solvers and automated theorem provers, as certificates of correctness for the answers they provide. These automated deduction tools have a wide range of application areas, from mathematics to software and hardware verification.

By providing smaller resolution proofs that are easier and faster to check, Skeptik aims at improving the reliability of these automated deduction tools and at facilitating the exchange of information between them.


###Usage Instructions###

Tell your java virtual machine to use UTF-8 and more memory. You can do this by setting your environment variable with the following command:

```
  $ export JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF-8 -Xmx1024m"
```

(you may increase the value after -Xmx, if you need to provide more memory to the JVM)

You must have [SBT](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) (version >= 0.13) installed. SBT automatically downloads all compilers and libraries on which  Skeptik depends.

To start SBT, go to Skeptik's home folder using the terminal and run:

```
  $ sbt
```

You can run skeptik within SBT's command line. With the following SBT command, skeptik will be compiled and executed, and a help message will be shown.

```  
  > skeptik --help
```

The following command processes the proof 'eq_diamond9.smt2' using the algorithms 'RP' and the sequential composition of 'D', 'RPI' and 'LU'. The compressed proofs are written using the 'smt2' proof format.
  

```
  > skeptik -a RP -a (D*RPI*LU) -f smt2 examples/proofs/VeriT/eq_diamond9.smt2
```

To use skeptik from outside SBT, a jar file must be generated by executing the following command within SBT:

```
  > one-jar
```

The generated jar file can be deployed and used like any other java jar file:

```
  $ java -jar skeptik.jar -a RP -a (D*RPI*LU) -f smt2 examples/proofs/VeriT/eq_diamond9.smt2
```


###Skeptik as a Library###

To import Skeptik as a library in your own Java or Scala projects,  
add the following line to your build.sbt file, if you are using [SBT](https://github.com/harrah/xsbt/wiki/Getting-Started-Setup) to manage the library dependencies of your project:

```
   libraryDependencies += "at.logic" %% "skeptik" % "1.0"
```

or add the following lines to your pom.xml file, if you are using [Maven](http://maven.apache.org/) instead:

```
        <dependency>
            <groupId>at.logic</groupId>
            <artifactId>skeptik_2.10</artifactId>
            <version>1.0</version>
        </dependency>
```



###Stats###

[![Build Status](https://buildhive.cloudbees.com/job/Paradoxika/job/Skeptik/badge/icon)](https://buildhive.cloudbees.com/job/Paradoxika/job/Skeptik/)
[![Ohloh](https://www.ohloh.net/p/Skeptik/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/Skeptik)




###Development Policy###

Developers are encouraged to:
 
 1. fork from [Paradoxika/Skeptik](https://github.com/Paradoxika/Skeptik), 
 2. program according to the [usual Scala style conventions](http://docs.scala-lang.org/style/),
 3. follow the [git flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model in their own forks, 
 4. make pull requests when they have finished a feature or hotfix branch.

Using the git flow model can be easier with the [gitflow extension for git](https://github.com/nvie/gitflow).



###Documentation###

Detailed theoretical descriptions of some of the algorithms implemented in Skeptik are available in our [papers](https://github.com/Paradoxika/Papers). There are also [slides](https://github.com/Paradoxika/Talks) of presentations about Skeptik.

Scaladoc documentation for the code can be generated by running the following command:

```
  sbt doc
```



###Active Developers###

 * [Bruno Woltzenlogel Paleo](https://github.com/Ceilican/Skeptik)
 * [Joseph Boudou](https://github.com/Jogo27/ResK-GSoC)
 * [Andreas Fellner](https://github.com/AFellner/Skeptik)

[Full list of contributors](https://www.ohloh.net/p/Skeptik/contributors?query=&sort=commits)



###Job Opportunities###

 * [Google Summer of Code](https://github.com/Paradoxika/Skeptik/wiki/GSoC-Instructions) grants are available every year. If you would like to apply, it is never too early to contact us. 
 * If you would like to do a project, M.Sc. or Ph.D. at [TU-Wien](http://www.tuwien.ac.at/en/tuwien_home/) related to Skeptik, please contact [Bruno Woltzenlogel Paleo](http://www.logic.at/people/bruno)


###Communication Channels###

 * [Skeptik's Mailinglist for Developers](https://groups.google.com/forum/?fromgroups#!forum/skeptik-dev)
 * IRC Channel on [freenode](http://freenode.net): \#Skeptik


###Support###
 
 * Skeptik's development is currently funded by the Austrian Science Fund ([Project P24300](http://www.fwf.ac.at/en/projects/projekt_datenbank.asp))
 * Skeptik was supported by [Google Summer of Code 2013](http://www.google-melange.com/gsoc/project/google/gsoc2013/afellner/28001)
 * Skeptik was supported by [Google Summer of Code 2012](http://www.google-melange.com/gsoc/project/google/gsoc2012/josephboudou/17001)
 * A free license of [YourKit](http://www.yourkit.com/) profiler helps us keep Skeptik's code fast and leak-free
 * Fast experiments on thousands of large proofs are made possible by the [Vienna Scientific Cluster](http://vsc.ac.at/)
 
 