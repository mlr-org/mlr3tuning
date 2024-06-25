# initializing TuningInstanceAsyncSingleCrit works

    Code
      print(instance)
    Output
      <TuningInstanceAsyncMultiCrit>
      * State:  Not optimized
      * Objective: <ObjectiveTuningAsync:classif.rpart_on_pima>
      * Search Space:
             id    class lower upper nlevels
         <char>   <char> <num> <num>   <num>
      1:     cp ParamDbl  0.01   0.1     Inf
      * Terminator: <TerminatorEvals>
      * Workers: 0

# TuningInstanceAsyncSingleCrit can be passed to a tuner

    Code
      print(instance)
    Output
      <TuningInstanceAsyncMultiCrit>
      * State:  Optimized
      * Objective: <ObjectiveTuningAsync:classif.rpart_on_pima>
      * Search Space:
             id    class lower upper nlevels
         <char>   <char> <num> <num>   <num>
      1:     cp ParamDbl  0.01   0.1     Inf
      * Terminator: <TerminatorEvals>
      * Result:
                 cp classif.ce classif.acc
              <num>      <num>       <num>
      1: 0.01862614   0.234375    0.765625
      * Archive:
                 cp classif.ce classif.acc
              <num>      <num>       <num>
      1: 0.06412381  0.2630208   0.7369792
      2: 0.04819687  0.2486979   0.7513021
      3: 0.08161351  0.2630208   0.7369792
      4: 0.01862614  0.2343750   0.7656250
      * Workers: 2

