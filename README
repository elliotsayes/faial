# Usage

```bash
$ make
```

## Example

```bash
$ ./main < examples/customIntegrator.proto 
Location: tempBuffer
Vars: idx, i, $TID2, $TID1
Pre: (< i 999)
(owned $TID2 (timed 999 (ro 0 1 (= idx $TID2))))
(owned $TID1 (timed 999 (ro 0 1 (= idx $TID1))))
(owned $TID2 (timed (+ 1 i) (rw $TID2 1 (= (mod $TID2 (* i 2)) 0))))
(owned $TID1 (timed (+ 1 i) (rw $TID1 1 (= (mod $TID1 (* i 2)) 0))))

Location: x
Vars: idx, $TID2, $TID1
Pre: true
(owned $TID2 (timed 0 (rw $TID2 1 true)))
(owned $TID1 (timed 0 (rw $TID1 1 true)))
(owned $TID2 (timed 0 (ro (* idx $TID2) 999 true)))
(owned $TID1 (timed 0 (ro (* idx $TID1) 999 true)))
```
