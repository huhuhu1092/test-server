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
	// 0 is the base color texture
    GLint getTextureCoordAttributeLoc(int index);
    GLint getColorUnifyLoc()
    {
        return m_u_color_loc;
    }
    GLint getWorldViewPerspectiveMatrixUnifyLoc()
    {
        return m_u_wvp_matrix_loc;
    }
	//index start from 0
	// 0 is the base color texture
    GLint getTextureUnifyLoc(int index);
    GLint getTexCombineModeUnifyLoc()
    {
        return m_u_tex_combine_mode_loc;
    }
	// index start from 1
	GLint getTexCoordSameAsTex0(int index);
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
    GLint m_u_color_loc;
	GLint m_u_wvp_matrix_loc;
    GLint m_u_tex1_coord_same_as_tex0;
    GLint m_u_tex2_coord_same_as_tex0;
	GLint m_u_tex3_coord_same_as_tex0;
	GLint m_u_colora;
	GLint m_u_colorr;
	GLint m_u_colorg;
	GLint m_u_colorb;
    char* mVertexShaderSrc;
    char* mFragmentShaderSrc;
};
#endif
