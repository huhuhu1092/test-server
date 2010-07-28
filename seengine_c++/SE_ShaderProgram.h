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
    GLunit getHandler();
    bool initOK();
    void use();
    GLint getPositionAttributeLoc()
    {
        return m_a_position_loc;
    }
    GLint getBaseColorTexCoordAttributeLoc()
    {
        return m_a_tex_coord_loc;
    }
    GLint getColorUnifyLoc()
    {
        return m_u_color_loc;
    }
    GLint getWorldViewPerspectiveMatrixUnifyLoc()
    {
        return m_u_wvp_matrix_loc;
    }
    GLint getBaseColorTextureUnifyLoc()
    {
        return m_u_basecolor_texture_loc;
    }
    GLint getShaderModeUnifyLoc()
    {
        return m_u_shading_mode_loc;
    }
protected:
    void link();
private:
    GLuint mShaderProgramObject;
    bool mHasInit;
    GLint m_a_position_loc;
    GLint m_a_tex_coord_loc;
    GLint m_u_basecolor_texture_loc;
    GLint m_u_shading_mode_loc;
    GLint m_u_color_loc;
	GLint m_u_wvp_matrix_loc;
    
};
#endif
