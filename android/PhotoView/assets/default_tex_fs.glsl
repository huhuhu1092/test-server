precision mediump float;
varying vec2 vTextureCoord;
uniform sampler2D muTex1;
void main() 
{
    vec4 color = texture2D(muTex1, vTextureCoord);
    gl_FragColor = color;
}