#include <cstdlib>
#include <iostream>
#include <getopt.h>
#include <ImageMagick-7/Magick++.h>

void printHelp(){
  std::cout<< "help massage" << std::endl;
}

int main (int argc, char *argv[]) {
  int c;
  int rampGainUp = 5;
  int rampGainDown = 5;
  int mass = 10;
  int frequency = 10;
  bool displayImg = false;
  while ((c = getopt(argc, argv, "hgGfbIFmiod:")) != -1) {
    switch(c) {
      case 'h':
        printHelp();
        exit(0);
      case 'g':
        rampGainUp = atoi(optarg);
        break;
      case 'G':
        rampGainDown = atoi(optarg);
        break;
      case 'f':
        frequency = atoi(optarg);
        break;
      case 'b': //background colour WIP
        break;
      case 'I': //image colour
        break;
      case 'F': //foreground colour
        break; 
      case 'm':
        mass = atoi(optarg);
        break; 
      case 'i': //input file
        break; 
      case 'o': //output file
        break; 
      case 'd':
        displayImg = true;
        break; 
      default:
        printHelp();
        exit(0);
    }
  }
  std::cout<< "mass: " << mass <<std::endl;

 
  return 0;
}
