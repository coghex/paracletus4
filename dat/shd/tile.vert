#version 450
#extension GL_ARB_separate_shader_objects : enable

// see https://www.khronos.org/opengl/wiki/Interface_Block_(GLSL) for
// difference between block name and instance name
layout(binding = 0) uniform TransformationObject {
  mat4 model;
  mat4 view;
  mat4 proj;
} trans;

layout (binding = 2) buffer DynTransObject {
  mat4 move[20000];
} dyn;

layout (binding = 3) buffer DynTexTransObject {
  mat4 dynTexI[20000];
} dynTex;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inColor;
layout(location = 2) in vec3 inTexCoord;
layout(location = 3) in vec3 inMove;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out int fragTexIndex;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    int bufI = int(floor(inMove.x - 0.5));
    int dynI = int(floor(inMove.y - 0.5));

    vec4 col0 = vec4(1.0,0.0,0.0,0.0);
    vec4 col1 = vec4(0.0,1.0,0.0,0.0);
    vec4 col2 = vec4(0.0,0.0,1.0,0.0);
    vec4 col3 = vec4(0.0,0.0,-1.0,1.0);
    mat4 basicI = mat4(col0,col1,col2,col3);
    mat4 view = (inMove.z > 0.0) ? trans.view : basicI;

    vec4 col4 = vec4(-1*trans.view[3][2],0.0,0.0,0.0);
    vec4 col5 = vec4(0.0,-1*trans.view[3][2],0.0,0.0);
    vec4 col6 = vec4(0.0,0.0,1.0,0.0);
    vec4 col7 = vec4(0.0,0.0,0.0,1.0);
    mat4 zoom = mat4(col4,col5,col6,col7);
    mat4 proj = (inMove.z > 0.0) ? trans.proj * zoom : trans.proj;
    
    mat4 dynV = (inMove.y > 0.0) ? (trans.model * (dyn.move[dynI])) : trans.model;
    gl_Position = proj * view * dynV * vec4(inPosition, 1.0);
    int inTex = int(inTexCoord.z);
    mat4 dynTC = dynTex.dynTexI[dynI];
    vec4 dynColor = vec4(dynTC[0][3] / 255,dynTC[1][3] / 255,dynTC[2][3] / 255,dynTC[3][3] / 255);
    fragColor = (inMove.x > 0.0) ? inColor * dynColor : inColor;
    int texI = int(floor(dynTC[3][2]));
    fragTexCoord = (inMove.y > 0.0) ? (vec2 (inTexCoord.x + ((dynTC[3][0])/32.0), inTexCoord.y + ((dynTC[3][1])/32.0))) : inTexCoord.xy;
    fragTexIndex = (inMove.x > 0.0) ? inTex + texI : inTex;
}
