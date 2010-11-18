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
    GLuint getHandler();
    void recreate();
    bool initOK();
    void use();
    GLint getPositionAttributeLoc()
    {
        return m_a_position_loc;
    }
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
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
    GLint m_u_color_loc;
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
	GLint getShadingModeUniformLoc()
	{
		return m_u_shading_mode_loc;
	}
protected:
	virtual void link();
private:
	GLint m_a_tex_coord_loc;
	GLint m_u_texture_loc;
	GLint m_u_shading_mode_loc;
};
class SE_SurfaceShaderProgram : public SE_ShaderProgram
{
	DECLARE_OBJECT(SE_SurfaceShaderProgram)
public:
	SE_SurfaceShaderProgram();
	SE_SurfaceShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc);
	~SE_SurfaceShaderProgram();
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
#endif
