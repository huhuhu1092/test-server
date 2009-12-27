#include "CreateMessageDefine.h"
#include <QFile>
int main(int argc, char** argv)
{
    CreateMessageDefine md("SMessageDefine.h");
    QFile file("messagedefine.xml");
    if(!file.open(QFile::ReadOnly))
        return -1;
    md.read(&file);
    md.save();
}
