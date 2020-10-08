#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void rep(char x[], int y, FILE *file);

int main(int argc, char *argv[]) {
	// use file names vecAdd.cu for no data race kernel.
	FILE *out_file = fopen("vectorAdd.cu", "w");
	if (out_file == NULL) {
		printf("ERROR IN FILE");
	}

	// use file names vecAdd-race.cu for data race kernels
	FILE *race_file = fopen("vectorAdd-race.cu", "w");
	if (race_file == NULL) {
		printf("ERROR IN FILE");
	}

	// take in command line arguments for number of loops, nested or sequential and loop length.
	int n = atoi(argv[1]);		// number of loops
	char* option = argv[2];		// NE for nested loops and SE for sequential loops
	int N = atoi(argv[3]);		// loop length i.e N in vecAdd code

	char tab[] = "  ";
	int nest = 0;


	if (!strcmp(option, "NE")) {
		// +++++++++++++++ Print nested n loops of length N without a data race to file ++++++++++++++++
		fprintf(out_file, "//pass\n");
		fprintf(out_file, "//--blockDim=[64,64] --gridDim=[1,1]\n");
		fprintf(out_file, "\n#include <cuda.h>\n");
		fprintf(out_file, "\n__global__ void vectorAdd(float *out, float *a, float *b)\n");
		fprintf(out_file, "{\n");
		for (int i = 1; i <= n; i++) {
			rep(tab, i, out_file);
			fprintf(out_file, "for (int %c%d = 0; %c%d < %d; %c%d++)\n", 'i', nest, 'i', nest, N, 'i', nest);
			rep(tab, i, out_file);
			fprintf(out_file, "{\n");
			rep(tab, i+1, out_file);
       		fprintf(out_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, i+1, out_file);
        	fprintf(out_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, i+1, out_file);
        	fprintf(out_file, "__syncthreads();\n");
        	nest++;
		}
		rep(tab, 1, out_file);
		rep("}", n, out_file);
		fprintf(out_file, "\n}\n");

		nest = 0;
		// +++++++++++++++ Print nested n loops of length N with a data race to file ++++++++++++++++
		fprintf(race_file,"//pass\n");
		fprintf(race_file, "//--blockDim=[64,64] --gridDim=[1,1]\n");
		fprintf(race_file, "\n#include <cuda.h>\n");
		fprintf(race_file, "\n__global__ void vectorAdd(float *out, float *a, float *b)\n");
		fprintf(race_file, "{\n");
		for (int i = 1; i <= n; i++) {
			rep(tab, i, race_file);
			fprintf(race_file, "for (int %c%d = 0; %c%d < %d; %c%d++)\n", 'i', nest, 'i', nest, N, 'i', nest);
			rep(tab, i, race_file);
			fprintf(race_file, "{\n");
			rep(tab, i+1, race_file);
        	fprintf(race_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, i+1, race_file);
        	fprintf(race_file, "__syncthreads();\n");
        	rep(tab, i+1, race_file);
        	fprintf(race_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	nest++;
		}
		rep(tab, 1, race_file);
		rep("}", n, race_file);
		fprintf(race_file, "\n}\n");
	}

	if (!strcmp(option, "SE")) {
		// +++++++++++++++ Print sequential n loops of length N without a data race to file ++++++++++++++++
		fprintf(out_file, "//pass\n");
		fprintf(out_file, "//--blockDim=[64,64] --gridDim=[1,1]\n");
		fprintf(out_file, "\n#include <cuda.h>\n");
		fprintf(out_file, "\n__global__ void vectorAdd(float *out, float *a, float *b)\n");
		fprintf(out_file, "{\n");
		for (int i = 1; i <= n; i++) {
			rep(tab, 1, out_file);
			fprintf(out_file, "for (int %c%d = 0; %c%d < %d; %c%d++)\n", 'i', nest, 'i', nest, N, 'i', nest);
			rep(tab, 1, out_file);
			fprintf(out_file, "{\n");
			rep(tab, 2, out_file);
       		fprintf(out_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, 2, out_file);
        	fprintf(out_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, 2, out_file);
        	fprintf(out_file, "__syncthreads();\n");
        	nest++;
        	rep(tab, 1, out_file);
        	fprintf(out_file, "}\n");
		}
		fprintf(out_file, "\n}\n");

		nest = 0;
		// +++++++++++++++ Print sequential n loops of length N with a data race to file ++++++++++++++++
		fprintf(race_file,"//pass\n");
		fprintf(race_file, "//--blockDim=[64,64] --gridDim=[1,1]\n");
		fprintf(race_file, "\n#include <cuda.h>\n");
		fprintf(race_file, "\n__global__ void vectorAdd(float *out, float *a, float *b)\n");
		fprintf(race_file, "{\n");
		for (int i = 1; i <= n; i++) {
			rep(tab, 2, race_file);
			fprintf(race_file, "for (int %c%d = 0; %c%d < %d; %c%d++)\n", 'i', nest, 'i', nest, N, 'i', nest);
			rep(tab, 2, race_file);
			fprintf(race_file, "{\n");
			rep(tab, 2, race_file);
        	fprintf(race_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	rep(tab, 2, race_file);
        	fprintf(race_file, "__syncthreads();\n");
        	rep(tab, 2, race_file);
        	fprintf(race_file, "out[threadIdx.x + %c%d] = a[%c%d] + b[%c%d];\n", 'i', nest, 'i', nest, 'i', nest);
        	nest++;
        	rep(tab, 1, race_file);
        	fprintf(race_file, "}\n");
		}
		fprintf(race_file, "\n}\n");

	}
	
	return 0;
}

// helper function to print a char or string repeated number of times.
void rep(char x[], int y, FILE *file) {
	for (int i = 0; i < y; i++) {
		fprintf(file, "%s", x);
	}

}
