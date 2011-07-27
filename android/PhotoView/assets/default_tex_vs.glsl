uniform mat4 muMVPMatrix;
attribute vec4 maPosition;
attribute vec2 maTexCoord;
varying vec2 vTextureCoord;
void main() 
{
    gl_Position = muMVPMatrix * maPosition;
    vTextureCoord = maTexCoord;
}