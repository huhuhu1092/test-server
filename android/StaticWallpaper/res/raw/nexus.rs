// Copyright (C) 2009 The Android Open Source Project
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#pragma version(1)
#pragma stateVertex(PVOrtho)
#pragma stateFragment(PFTexture)
#pragma stateStore(PSSolid)

#define MAX_PULSES           20
#define MAX_EXTRAS           40
#define PULSE_SIZE           14 // Size in pixels of a cell
#define HALF_PULSE_SIZE      7
#define GLOW_SIZE            64 // Size of the leading glow in pixels
#define HALF_GLOW_SIZE       32
#define SPEED                0.2f // (200 / 1000) Pixels per ms
#define SPEED_VARIANCE       0.3f
#define PULSE_NORMAL         0
#define PULSE_EXTRA          1
#define TRAIL_SIZE           40 // Number of cells in a trail
#define MAX_DELAY	         2000 // Delay between a pulse going offscreen and restarting


int gNow;


void setColor(int c) {
    if (c == 0) {
        // red
        color(1.0f, 0.0f, 0.0f, 1.0f);
    } else if (c == 1) {
        // green
        color(0.0f, 0.6f, 0.0f, 1.0f);
    } else if (c == 2) {
        // blue
        color(0.0f, 0.4f, 0.8f, 1.0f);
    } else if (c == 3) {
        // yellow
        color(1.0f, 0.8f, 0.0f, 1.0f);
    }
}

void drawBackground(int width, int height) {
  bindTexture(NAMED_PFTexture, 0, NAMED_TBackground);
    color(1.0f, 1.0f, 1.0f, 1.0f);
    if (State->rotate) {
        drawRect(0.0f, 0.0f, height * 2, width, 0.0f);
    } else {
    	drawRect(0.0f, 0.0f, width * 2, height, 0.0f);
   	}
}


int main(int index) {

    gNow = uptimeMillis();

    if (Command->command != 0) {
        debugF("x", Command->x);
        debugF("y", Command->y);
        Command->command = 0;
    }
    else
    {
/*
        debugF("x", Command->x);
        debugF("y", Command->y);
*/
    }

    int width = State->width;
    int height = State->height;

    float matrix[16];
    matrixLoadIdentity(matrix);
/*
    debugF("x", State->xOffset);
*/
    if (State->rotate) {
        //matrixLoadRotate(matrix, 90.0f, 0.0f, 0.0f, 1.0f);
        //matrixTranslate(matrix, 0.0f, -height, 1.0f);
    } else {
         debugF("x", State->xOffset);
         matrixTranslate(matrix, -(State->xOffset * width), 0, 0);
    }

    vpLoadModelMatrix(matrix);

    drawBackground(width, height);
    return 45;
}
