#include <cstdlib>
#include <string.h>
#include <iostream>
#include <unistd.h>
#include <Magick++.h>
#include <string>

void printHelp(){
  std::cout<< "help massage" << std::endl;
}

int main (int argc, char *argv[]) {
  Magick::InitializeMagick(*argv);
  int c;
  int rampGainUp = 5;
  int rampGainDown = 5;
  int mass = 10;
  int frequency = 10;
  bool displayImg = false;
  char inFile[256];
  memset(inFile,0, sizeof(inFile));
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
      case 'i': //input file TODO fix
        std::cout<< "1" << std::endl;
        strcpy(inFile, optarg);
        std::cout<< "2" << std::endl;
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
  Magick::Image argImage;
  std::cout<< "pointer:" << inFile[0] << std::endl; //need to try without getopt, not working as intented
  try{
    std::cout<< "3" << std::endl;
    argImage.read(inFile);
    argImage.display();
  }
  catch(...) {
    printHelp();
    exit(1);
  }
  std::cout<< "mass: " << mass <<std::endl;

 
  return 0;
}
