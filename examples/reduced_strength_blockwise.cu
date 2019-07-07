# --blockDim=256 --gridDim=2 -DWIDTH=2064 --no-inline

#|
 | This kernel demonstrates a blockwise strength-reduction loop.
 | Each block is given a disjoint partition (of length WIDTH) of A.
 | Then each thread writes multiple elements in the partition.
 | It is not necessarily the case that WIDTH%blockDim.x == 0
 |#

shared A;
const
#    global blockDim.x,
    local blockIdx.x,
    local threadIdx.x,
    global WIDTH;

assert distinct [threadIdx.x][blockIdx.x];
#assert blockDim.x <= WIDTH;
assert threadIdx.x < WIDTH;

#  __assert(__mod_pow2(WIDTH, blockDim.x) == 0);

foreach i < WIDTH {
    assert i >= threadIdx.x;
    assert (i - threadIdx.x) % blockIdx.x == 0;
    rw A[blockIdx.x*WIDTH+i];
}

