#include "SE_Vector.h"
#include "SE_Matrix.h"
int main(int argc, char** argv)
{
    SE_Application app("config.lua");
    app.setUpEnv();
    app.run();
    return 0;
}
