#include <stdio.h>
#include <math.h>
#include "./ase/aselib.h"
#include "SE_ResourceManager.h"
int main(int argc, char** argv)
{
    float t = sqrtf(1.0);
    if(argc < 3)
        return 1;
    ASE_Loader loader(argv[1], 0, 0);
    loader.Load();
    loader.Write(argv[2]);
    SE_ResourceManager resourceManager;
    SE_ResourceManager_InitFromFile(&resourceManager, "/home/luwei/program/seengine", argv[2]);
    return 0;
}
