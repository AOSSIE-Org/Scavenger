
![Logo](https://raw.github.com/Paradoxika/Skeptik/develop/res/logo150.png)

Scavenger is an automated theorem prover based on the new **conflict resolution** calculus,
which lifts some aspects of the conflict-drive clause learning approach used by sat-solvers to
logics with quantifiers. 


### Usage Instructions

Tell your java virtual machine to use UTF-8 and more memory. You can do this by setting your environment variable with the following command:

```
  $ export JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF-8 -Xmx1024m -Xss4m -XX:MaxPermSize=256m"
```

(you may change the values after ```-Xmx```, ```Xss``` and ```XX:MaxPermSize=``` to suit your needs)

You must have [SBT](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) (version >= 0.13) installed. SBT automatically downloads all compilers and libraries on which Scavenger depends.

To start SBT, go to Scavenger's home folder using the terminal and run:

```
  $ sbt
```

You can run scavenger within SBT's command line. With the following SBT command, scavenger will be compiled and executed, and a help message will be shown.

```  
  > scavenger --help
```

The following command runs scavenger on the 'examples/problems/CNF/SET006-1.cnfp'. The proof is written to 'refutation.proof'.
  

```
  > scavenger -a CR -f cnfp -o refutation.proof examples/problems/CNF/SET006-1.cnfp
```

To use scavenger from outside SBT, a jar file must be generated by executing the following command within SBT:

```
  > one-jar
```

The generated jar file can be deployed and used like any other java jar file:

```
  $ java -jar scavenger.jar -a CR -f cnfp -o refutation.proof examples/problems/CNF/SET006-1.cnfp
```




### Stats

[![Build Status](https://buildhive.cloudbees.com/job/Paradoxika/job/Skeptik/badge/icon)](https://buildhive.cloudbees.com/job/Paradoxika/job/Skeptik/)
[![Ohloh](https://www.ohloh.net/p/Skeptik/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/Skeptik)



### Documentation

Detailed theoretical descriptions of some of the algorithms implemented in Skeptik are available in our [papers](https://github.com/Paradoxika/Papers). There are also [slides](https://github.com/Paradoxika/Talks) of presentations about Skeptik.

Scaladoc documentation for the code can be generated by running the following command:

```
  sbt doc
```



### Active Developers

 * [Bruno Woltzenlogel Paleo](https://github.com/Ceilican/)
 * [Daniyar Itegulov](https://github.com/itegulov)
 * [Ezequiel Itegulov](https://github.com/EzequielPostan)


### Job Opportunities

 * [Google Summer of Code]() grants are available every year. If you would like to apply, it is never too early to contact us. 
 * If you would like to do a project, M.Sc. thesis or Ph.D. thesis related to Scavenger, please contact [Bruno Woltzenlogel Paleo](http://paleo.woltzenlogel.org)


### Communication Channels

 * [Scavenger's Mailinglist for Developers](https://groups.google.com/forum/?fromgroups#!forum/skeptik-dev)


### Licenses
 
* GNU-GPL-3.0

* CC-By-NC-SA [![License](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)](http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US)

 

