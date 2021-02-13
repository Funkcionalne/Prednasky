#include "stdio.h"

float first(float (*f)(float) ) { 
  return (*f)(1.0)+2.0; 
  return f(1.0)+2.0; // alebo
} 

float second(float x) {
  return (x/2.0);
}

int main(void) {
    // Disable stdout buffering
    setvbuf(stdout, NULL, _IONBF, 0);
    printf("%f\n",first(&second));
    return 0;
}

