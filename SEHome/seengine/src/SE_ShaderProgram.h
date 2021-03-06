#ifndef SE_SHADERPROGRAM_H
#define SE_SHADERPROGRAM_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#endif
#include "SE_Object.h"
class SE_ShaderProgram : public SE_Object
{
	DECLARE_OBJECT(SE_ShaderProgram)
public:
	SE_ShaderProgram();
    SE_ShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
    virtual ~SE_ShaderProgram();
    virtual void releaseHardwareResource();
	void create(char* vertexShader, char* fragmentShader);
    void setSource(char* vertexShaderSrc, char* fragmentShaderSrc);
    GLuint getHandler();
    void recreate();
    bool initOK();
    void use();
	void validate();
    GLint getPositionAttributeLoc()
    {
        return m_a_position_loc;
    }
    GLint getWorldViewPerspectiveMatrixUniformLoc()
    {
        return m_u_wvp_matrix_loc;
    }
protected:
    virtual void link();
    void init(char* vertexShaderSrc, char* fragmentShaderSrc);
protected:
    GLuint mShaderProgramObject;
    bool mHasInit;
    GLint m_a_position_loc;
	GLint m_u_wvp_matrix_loc;
    char* mVertexShaderSrc;
    char* mFragmentShaderSrc;
};
class SE_SimpleSurfaceShaderProgram : public SE_ShaderProgram
{
	DECLARE_OBJECT(SE_SimpleSurfaceShaderProgram)
public:
	SE_SimpleSurfaceShaderProgram();
	SE_SimpleSurfaceShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
	~SE_SimpleSurfaceShaderProgram();
    GLint getTexCoordAttributeLoc()
	{
		return m_a_tex_coord_loc;
	}
	GLint getTextureUniformLoc()
	{
		return m_u_texture_loc;
	}
	GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
	GLint getShadingModeUniformLoc()
	{
		return m_u_shading_mode_loc;
	}
protected:
	virtual void link();
private:
    GLint m_u_color_loc;
	GLint m_a_tex_coord_loc;
	GLint m_u_texture_loc;
	GLint m_u_shading_mode_loc;
};
class SE_ColorExtractShaderProgram : public SE_ShaderProgram
{
	DECLARE_OBJECT(SE_ColorExtractShaderProgram)
public:
	SE_ColorExtractShaderProgram();
	SE_ColorExtractShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
	~SE_ColorExtractShaderProgram();
	//index start from 0
    GLint getTextureUniformLoc(int index);
    GLint getTexCombineModeUniformLoc()
    {
        return m_u_tex_combine_mode_loc;
    }
	GLint getColorOpModeUniformLoc()
	{
		return m_u_color_op_mode_loc;
	}
	GLint getColorChannelIndexUniformLoc(int index);
	// index start from 1
	GLint getTexCoordIndexUniformLoc(int index);
	GLint getMarkColorUniformLoc(int index);
		// index start from 0
    GLint getTextureCoordAttributeLoc(int index);
protected:
	void link();
private:
    GLint m_a_tex_coord0_loc;
    GLint m_a_tex_coord1_loc;
    GLint m_a_tex_coord2_loc;
    GLint m_a_tex_coord3_loc;
    GLint m_u_texture0_loc;
	GLint m_u_texture1_loc;
	GLint m_u_texture2_loc;
	GLint m_u_texture3_loc;
    GLint m_u_tex_combine_mode_loc;
	GLint m_u_color_op_mode_loc;
	//tex_coord_index indicate which texture coordinate will be used in
	//this texture. for example: m_u_tex0_coord_index is 1 that means texture0 use m_a_tex_coord1
	GLint m_u_tex0_coord_index_loc;
    GLint m_u_tex1_coord_index_loc;
    GLint m_u_tex2_coord_index_loc;
	GLint m_u_tex3_coord_index_loc;
	GLint m_u_colora_loc;
	GLint m_u_colorr_loc;
	GLint m_u_colorg_loc;
	GLint m_u_colorb_loc;
	GLint m_u_rchannelindex_loc;
    GLint m_u_gchannelindex_loc;
	GLint m_u_bchannelindex_loc;
	GLint m_u_achannelindex_loc;
};

class SE_ColorEffectShaderProgram : public SE_ShaderProgram
{
	DECLARE_OBJECT(SE_ColorEffectShaderProgram)
public:
    SE_ColorEffectShaderProgram();
    GLint getTexCoordAttribLoc()
    {
        return m_a_tex_coord0;
    }
    // 0 : background
    // 1 : channel
    // 2 - 5 : texr, texg, texb, texa
    GLint getTextureUniformLoc(int index)
	{
		GLint ret = -1;
		switch(index)
		{
		case 0:
			ret = m_u_texture_background;
			break;
		case 1:
			ret = m_u_texture_channel;
			break;
		case 2:
			ret = m_u_texture_texr;
			break;
		case 3:
			ret = m_u_texture_texg;
			break;
		case 4:
			ret = m_u_texture_texb;
			break;
		case 5:
			ret = m_u_texture_texa;
			break;
		default:
			break;
		}
		return ret;
	}
	//0, 1, 2, 3 | r, g, b, a
	GLint getHasMarkUniformLoc(int index)
	{
        GLint ret = -1;
		switch(index)
		{
		case 0:
			ret = m_u_has_markr;
			break;
		case 1:
			ret = m_u_has_markb;
			break;
		case 2:
			ret = m_u_has_markg;
			break;
		case 3:
			ret = m_u_has_marka;
			break;
		}
		return ret;
	}

    // 0, 1, 2, 3 | r g b a
    GLint getHasTextureUniformLoc(int index)
	{
		GLint ret = -1;
		switch(index)
		{
		case 0:
			ret = m_u_has_texr;
			break;
		case 1:
			ret = m_u_has_texg;
			break;
		case 2:
			ret = m_u_has_texb;
			break;
		case 3:
			ret = m_u_has_texa;
			break;
		default:
			break;
		}
		return ret;
	}
    // 0, 1, 2, 3, 4 | markr markg markb marka backgroudnalpha
    GLint getMarkAlphaUniformLoc(int index)
	{
		switch(index)
		{
		case 0:
			return m_u_markr_alpha;
		case 1:
			return m_u_markg_alpha;
		case 2:
			return m_u_markb_alpha;
		case 3:
			return m_u_marka_alpha;
		case 4:
			return m_u_background_alpha;
		default:
			return -1;
		}
	}
    // 0, 1, 2, 3 | r g b a
    GLint getMarkFunctionUniformLoc(int index)
	{
		switch(index)
		{
		case 0:
			return m_u_markr_fn;
		case 1:
			return m_u_markg_fn;
		case 2:
			return m_u_markb_fn;
		case 3:
			return m_u_marka_fn;
		default:
			return -1;
		}
	}
    // 0, 1, 2, 3 | r g b a
    GLint getMarkColorUniformLoc(int index)
	{
		switch(index)
		{
		case 0:
			return m_u_colorr;
		case 1:
			return m_u_colorg;
		case 2:
			return m_u_colorb;
		case 3:
			return m_u_colora;
		default:
			return -1;
		}
	}
	GLint getTextureFnUniformLoc(int index)
	{
	    switch(index)
		{
		case 0:
	        return m_u_texr_fn;
		case 1:
	        return m_u_texg_fn;
		case 2:
	        return m_u_texb_fn;
		case 3:
	        return m_u_texa_fn;
		default:
			return -1;
		}
	}
	GLint getBackgroundAlphaUniformLoc()
	{
		return m_u_background_alpha;
	}
protected:
    virtual void link();
private:
    GLint m_a_tex_coord0;
    GLint m_u_texture_background;
    GLint m_u_texture_channel;
    GLint m_u_texture_texr;
    GLint m_u_texture_texg;
    GLint m_u_texture_texb;
    GLint m_u_texture_texa;
    GLint m_u_has_texr;
    GLint m_u_has_texg;
    GLint m_u_has_texb;
    GLint m_u_has_texa;
	GLint m_u_has_markr;
	GLint m_u_has_markg;
	GLint m_u_has_markb;
	GLint m_u_has_marka;
    GLint m_u_markr_alpha;
    GLint m_u_markg_alpha;
    GLint m_u_markb_alpha;
    GLint m_u_marka_alpha;
    GLint m_u_background_alpha;
    GLint m_u_markr_fn;
    GLint m_u_markg_fn;
    GLint m_u_markb_fn;
    GLint m_u_marka_fn;
	GLint m_u_texr_fn;
	GLint m_u_texg_fn;
	GLint m_u_texb_fn;
	GLint m_u_texa_fn;
    GLint m_u_colorr;
    GLint m_u_colorg;
    GLint m_u_colorb;
    GLint m_u_colora;
};

#endif
