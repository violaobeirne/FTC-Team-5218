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

package softwareclass;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

@Autonomous(name = "AutonomousGampepadTest")
//@Disabled
public class AutonomousGamepadTest extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private FourWheelDirectDrivetrain drivetrain;

    private enum AllianceColor {
        BLUE, //BUTTON X
        RED, //BUTTON B
        DEFAULT
    }

    private enum RobotPosition{
        BUILD_SITE, //BUTTON Y
        DEPOT, //BUTTON A
        DEFAULT
    }

    //declaring gamepad variables
    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected RobotPosition robotPosition;

    //declaring telemetry item
    private Telemetry.Item allianceTlm;
    private Telemetry.Item positionTlm;


    //private static final int TICKS_PER_INCH = 79;

    @Override
    public void handleEvent(RobotEvent e)
    {
       if (e instanceof GamepadTask.GamepadEvent) {
           GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent)e;
           switch (event.kind) {
               case BUTTON_X_DOWN:
                   allianceColor = AllianceColor.BLUE;
                   allianceTlm.setValue("BLUE");
                   break;
               case BUTTON_B_DOWN:
                   allianceColor = AllianceColor.RED;
                   allianceTlm.setValue("RED");
                   break;
               case BUTTON_Y_DOWN:
                   robotPosition = RobotPosition.BUILD_SITE;
                   positionTlm.setValue("BUILD_SITE");
                   break;
               case BUTTON_A_DOWN:
                   robotPosition = RobotPosition.DEPOT;
                   positionTlm.setValue("DEPOT");
                   break;
           }
       }
    }

    @Override
    public void init()
    {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");

        //instantiate a FourWheelDirectDrivetrain
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        //initializing gamepad variables
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        //Telemetry setup
        telemetry.setAutoClear(false);
        allianceTlm = telemetry.addData("ALLIANCE", "Unselected (X=blue/B-red)");
        positionTlm = telemetry.addData("ALLIANCE", "Unselected (Y-build/A-depot)");
    }

    @Override
    public void start()
    {
        DeadReckonPath path = new DeadReckonPath();
        setAutonomousPath(path);
        this.addTask(new DeadReckonTask(this, path, drivetrain));

        //? this.addTask(new TankDriveTask(this, drivetrain));
    }

    private void setAutonomousPath(DeadReckonPath path){
        switch(robotPosition){
            case DEPOT:
                RobotLog.i ("Set Depot Path");
                path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 1.0);
                path.addSegment(DeadReckonPath.SegmentType.TURN, 90, 1.0);
                break;
            case BUILD_SITE:
                RobotLog.i ("Set Build Site Path");
                path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 1.0);
                path.addSegment(DeadReckonPath.SegmentType.TURN, 90, 1.0);
                path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 1.0);
                path.addSegment(DeadReckonPath.SegmentType.TURN, 90, 1.0);
                break;
            default:
                break;
        }
    }
}
