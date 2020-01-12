
/*
 * Copyright (c) September 2017 FTC Teams 25/5218
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted (subject to the limitations in the disclaimer below) provided that
 *  the following conditions are met:
 *
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer.
 *
 *  Redistributions in binary form must reproduce the above copyright notice, this
 *  list of conditions and the following disclaimer in the documentation and/or
 *  other materials provided with the distribution.
 *
 *  Neither the name of FTC Teams 25/5218 nor the names of their contributors may be used to
 *  endorse or promote products derived from this software without specific prior
 *  written permission.
 *
 *  NO EXPRESS OR IMPLIED LICENSES TO ANY PARTY'S PATENT RIGHTS ARE GRANTED BY THIS
 *  LICENSE. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESSFOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 *  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package test;

import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.util.ElapsedTime;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTask;

public class SkystoneDetectionTask extends RobotTask
{
    public enum EventKind {
        STONE_DETECTED,
    }

    public class SkystoneDetectionEvent extends RobotEvent
    {
        public EventKind kind;

        public SkystoneDetectionEvent(RobotTask task, EventKind kind)
        {
            super(task);
            this.kind = kind;
        }
    }

    protected ColorSensor colorSensor;
    protected int count;
    protected int threshold = 1000;
    protected int msDelay = 0;
    protected ElapsedTime delayTimer;

    public SkystoneDetectionTask(Robot robot, ColorSensor colorSensor)
    {
        super(robot);
        this.colorSensor = colorSensor;
    }

    @Override
    public void start()
    {
        delayTimer = new ElapsedTime(ElapsedTime.Resolution.MILLISECONDS);
        count = 0;
    }

    @Override
    public void stop()
    {
        robot.removeTask(this);
    }

    @Override
    public boolean timeslice()
    {
        SkystoneDetectionEvent event;

        int rgb = colorSensor.argb();
        int blue = colorSensor.blue();
        int red = colorSensor.red();
        int green = colorSensor.green();

        robot.telemetry.addData("Red  ", colorSensor.red());
        robot.telemetry.addData("Green", colorSensor.green());
        robot.telemetry.addData("Blue ", colorSensor.blue());
        RobotLog.i("ColorSensor 0x%08X %02X %02X %02X", rgb, red, green, blue);

        if (blue < threshold && red < threshold && green < threshold) {
            event = new SkystoneDetectionEvent(this, EventKind.STONE_DETECTED);
            robot.queueEvent(event);
        }
        return false;
    }
}
