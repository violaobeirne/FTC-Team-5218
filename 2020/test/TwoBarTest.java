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

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import opmodes.calibration.HisaishiCalibration;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

@TeleOp(name = "Two Bar Test")
@Disabled
public class TwoBarTest extends Robot {

    private DcMotor twoBar;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case RIGHT_BUMPER_DOWN:
                    twoBar.setPower(HisaishiCalibration.TBAR_ARM_UP);
                    break;
                case RIGHT_TRIGGER_DOWN:
                    twoBar.setPower(HisaishiCalibration.TBAR_ARM_DOWN);
                    break;
                case RIGHT_TRIGGER_UP:
                case RIGHT_BUMPER_UP:
                    twoBar.setPower(0.0);
                    break;
            }
        }
    }

    @Override
    public void init() {
        twoBar = hardwareMap.dcMotor.get("twoBar");
        {
            //make sure "LeftIntake" etc. aligns with configurations in phone or align "LeftIntake" w/phone
            GamepadTask gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
            addTask(gamepad);
        }
    }

    @Override
    public void start()
    {
    }

}
