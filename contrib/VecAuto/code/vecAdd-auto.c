#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void rep(char x[], int y, FILE *file);

int main(int argc, char *argv[]) {
	char filename[31] = "vectorAdd";
	strcat (filename, argv[1]);
	strcat (filename, "-auto.cu");
	FILE *out_file = fopen(filename, "w");
	if (out_file == NULL) {
		printf("ERROR IN FILE");
	}
	int n = atoi(argv[1]);
	char tab[] = "  ";
	int nest = 0;
	fprintf(out_file, "//pass\n");
	fprintf(out_file, "//--blockDim=[64,64] --gridDim=[1,1]\n");
	fprintf(out_file, "\n#include <cuda.h>\n");
	fprintf(out_file, "\n__global__ void vectorAdd(float *out, float *a, float *b, int N)\n");
	fprintf(out_file, "{\n");
	for (int i = 1; i <= n; i++) {
		rep(tab, i, out_file);
		fprintf(out_file, "for (int %c%d = 0; %c%d < N; %c%d++)\n", 'i', nest, 'i', nest, 'i', nest);
		rep(tab, i, out_file);
		fprintf(out_file, "{\n");
		rep(tab, i+1, out_file);
        fprintf(out_file, "out[%c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        rep(tab, i+1, out_file);
        fprintf(out_file, "out[%c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        rep(tab, i+1, out_file);
        fprintf(out_file, "__syncthreads();\n");
        nest++;
	}
	rep(tab, 1, out_file);
	rep("}", n, out_file);
	fprintf(out_file, "\n}\n");
	return 0;
}

void rep(char x[], int y, FILE *file) {
	for (int i = 0; i < y; i++) {
		fprintf(file, "%s", x);
	}

}
