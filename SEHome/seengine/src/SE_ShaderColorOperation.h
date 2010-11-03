#ifndef SE_SHADERCOLOROPERATION_H
#define SE_SHADERCOLOROPERATION_H
#include "SE_Common.h"
#include <vector>
class SE_ShaderColorOperation
{
public:
    SE_ShaderColorOperation();
    virtual ~SE_ShaderColorOperation()
    {}
    int getColorOperationMode()
    {
        return mColorOp;
    }
    void setColorOperationMode(int op)
    {
        mColorOp = op;
    }
	void setTextureMode(int texMode)
	{
		mTextureMode = texMode;
	}
	int getTextureMode()
	{
		return mTextureMode;
	}
    void getTextureModeColorOp(int* hasTexture, int num, int& outTexMode, int& outColorOp);
private:
    void initTextureModeProperty();
    static std::vector<int> textureModeProperty[SE_BLENDMODE_NUM];
    static bool textureModeInited;
    int mColorOp;
	int mTextureMode;
};
#endif
