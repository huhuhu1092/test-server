#ifndef SE_SHADERCOLOROPERATION_H
#define SE_SHADERCOLOROPERATION_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include <vector>
class SE_ShaderProperty
{
public:
	virtual ~SE_ShaderProperty() {}
};
class SE_ColorEffectShaderProperty : public SE_ShaderProperty
{
public:
	SE_ColorEffectShaderProperty()
	{
		for(int i = 0 ; i < 4 ; i++)
		{
			mHasTex[i] = 0;
			mHasMark[i] = 0;
			mTextureFn[i] = 0;
			mMarkAlpha[i] = 1.0f;
			mMarkFunction[i] = 0;
			mMarkTexture[i] = i + 2;
			mBackgroundAlpha = 1.0f;
			mBackgroundTexture = 0;
			mChannelTexture = 1;
		}

	}
	void setHasMark(int index, bool v)
	{
		mHasMark[index] = v;
	}
	bool getHasMark(int index)
	{
		return mHasMark[index];
	}
	void setHasTex(int index, bool v)
	{
		mHasTex[index] = v;
	}
	bool getHasTex(int index)
	{
		return mHasTex[index];
	}
	void setMarkAlpha(int index, float a)
	{
		mMarkAlpha[index] = a;
	}
	float getMarkAlpha(int index)
	{
		return mMarkAlpha[index];
	}
	void setBackgroundAlpha(float a)
	{
		mBackgroundAlpha = a;
	}
	float getBackgroudnAlpha()
	{
		return mBackgroundAlpha;
	}
	void setMarkFunction(int index, int fn)
	{
		mMarkFunction[index] = fn;
	}
	int getMarkFunction(int index)
	{
		return mMarkFunction[index];
	}
	int getTextureFn(int index)
	{
		return mTextureFn[index];
	}
	void setTextureFn(int index, int f)
	{
		mTextureFn[index] = f;
	}
	void setMarkColor(int index, const SE_Vector3f& c)
	{
		mMarkColor[index] = c;
	}
	SE_Vector3f getMarkColor(int index)
	{
		return mMarkColor[index];
	}
	void setBackgroundTexture(int texIndex)
	{
		mBackgroundTexture = texIndex;
	}
	int getBackgroundTexture()
	{
		return mBackgroundTexture;
	}
	void setChannelTexture(int texIndex)
	{
		mChannelTexture = texIndex;
	}
	int getChannelTexture()
	{
		return mChannelTexture;
	}
	void setMarkTexture(int index, int texIndex)
	{
		mMarkTexture[index] = texIndex;
	}
	int getMarkTexture(int index)
	{
		return mMarkTexture[index];
	}
private:
	bool mHasMark[4];
	bool mHasTex[4];// r , g , b, a
	float mMarkAlpha[4];//rgba
    float mBackgroundAlpha;
	int mMarkFunction[4];//rgba
	SE_Vector3f mMarkColor[4];//rgba
	int mBackgroundTexture;
	int mChannelTexture;
	int mMarkTexture[4];//rgba
	int mTextureFn[4];
};
class SE_ColorExtractShaderProperty : public SE_ShaderProperty
{
public:
    SE_ColorExtractShaderProperty();
    int getColorOperationMode()
    {
        return mColorOp;
    }
    void setColorOperationMode(int op)
    {
        mColorOp = op;
    }
	void setColorChannelIndex(int index, int v)
	{
		mColorChannelIndex[index] = v;
	}
	int getColorChannelIndex(int index)
	{
		return mColorChannelIndex[index];
	}
    //void getColorOpMode(int* hasTexture, int num, int& outTexMode, int& outColorOp);
private:
    int mColorOp;
	int mColorChannelIndex[4]; // r,g, b,a
};
#endif
