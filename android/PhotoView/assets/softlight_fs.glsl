precision mediump float;
varying vec2 vTextureCoord;
uniform sampler2D muTex1;
uniform sampler2D muTex2; 
//uniform float muInverseWidth; 
//uniform float muInverseHeight;
//uniform float muAlpha;
float getSoftLightColor(float c1, float c2)
{
    float c = 0.0;
    if(c2 <= 0.5)
    {
        c = (2.0 * c2 - 1.0) * (c1 - c1 * c1) + c1;
    }
    else
    {
        c = (2.0 * c2 - 1.0) * (sqrt(c1) - c1) + c1;
    }
    return c;
}
void main()
{
    vec4 c =  texture2D(muTex2, vTextureCoord);
    vec4 color1 = texture2D(muTex1, vTextureCoord);
    vec4 color2 = c;
    //vec4 color2 = texture2D(muTex2, vTextureCoord);
    float r = getSoftLightColor(color1.r, color2.r);
    float g = getSoftLightColor(color1.g, color2.g); 
    float b = getSoftLightColor(color1.b, color2.b);
    float a = getSoftLightColor(color1.a, color2.a);
    //gl_FragColor = color2 + 0.001 * color1;
    gl_FragColor = vec4(r, g , b , 1.0);
}