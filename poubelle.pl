%.......................................
% square
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

/*
square([[M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],1,M).

square([[_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],2,M).

square([[_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],3,M).

square([[_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],4,M).

square([[_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],5,M).

square([[_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],6,M).

square([[_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],7,M).

square([[_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],8,M).
        
square([[_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],9,M).

square([[_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],10,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],11,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],12,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],13,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],14,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],15,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],16,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],17,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],18,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],19,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],20,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],21,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],22,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],23,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],24,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],25,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],26,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],27,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],28,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],29,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],30,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_]],31,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_]],32,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_]],33,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_]],34,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_]],35,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_]],36,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_]],37,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_]],38,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_]],39,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_]],40,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_]],41,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M]],42,M).
*/