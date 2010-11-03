#ifndef SE_SHADERPROGRAM_H
#define SE_SHADERPROGRAM_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#endif
class SE_ShaderProgram
{
public:
    SE_ShaderProgram(char* vertexShader, char* fragmentShader);
    ~SE_ShaderProgram();
    GLuint getHandler();
    void releaseHardwareResource();
    void init();
    bool initOK();
    void use();
    GLint getPositionAttributeLoc()
    {
        return m_a_position_loc;
    }
	// index start from 0
    GLint getTextureCoordAttributeLoc(int index);
    GLint getColorUniformLoc()
    {
        return m_u_color_loc;
    }
    GLint getWorldViewPerspectiveMatrixUniformLoc()
    {
        return m_u_wvp_matrix_loc;
    }
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
	// index start from 1
	GLint getTexCoordIndexUniformLoc(int index);
	GLint getMarkColorUniformLoc(int index);
protected:
    void link();
    void init(char* vertexShaderSrc, char* fragmentShaderSrc);
private:
    GLuint mShaderProgramObject;
    bool mHasInit;
    GLint m_a_position_loc;
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
    GLint m_u_color_loc;
	GLint m_u_wvp_matrix_loc;
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
    char* mVertexShaderSrc;
    char* mFragmentShaderSrc;
};
#endif
