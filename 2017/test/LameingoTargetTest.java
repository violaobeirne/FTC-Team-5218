package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.Robot_MecanumDrive;
import team25core.Robot_Navigation;
import team25core.Robot_TwoWheelDrive;

/*
 * FTC Team 25: cmacfarl, January 31, 2017
 */

@Autonomous(name = "TEST Target", group = "AutoTest")
public class LameingoTargetTest extends Robot {

    enum TargetState {
        FIND_TARGET,
        LOST_TARGET,
        INITIAL_APPROACH,
        AT_TARGET,
        ALIGNED,
    };

    Robot_Navigation nav;
    Robot_TwoWheelDrive drive;
    DcMotor leftMotor;
    DcMotor rightMotor;
    TargetState state;
    double bearing;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init()
    {
        leftMotor = hardwareMap.dcMotor.get("leftMotor");
        rightMotor = hardwareMap.dcMotor.get("rightMotor");
        leftMotor.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        rightMotor.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        drive = new Robot_TwoWheelDrive(rightMotor, leftMotor);
        drive.initDrive(this);
        nav = new Robot_Navigation();
        nav.initVuforia(this, drive);

        nav.activateTracking();
    }

    @Override
    public void start()
    {
        state = TargetState.FIND_TARGET;
    }

    @Override
    public void loop()
    {
        super.loop();

        nav.targetsAreVisible();
        // nav.addNavTelemetry();

        switch (state) {
        case FIND_TARGET:
            if (nav.targetsAreVisible()) {
                drive.rotateRobot(0.0);
                nav.addNavTelemetry();
                // state = TargetState.INITIAL_APPROACH;
            } else {
                this.telemetry.addData("Rotating...", "");
                drive.rotateRobot(0.06);
            }
            break;
        case LOST_TARGET:
            if (nav.targetsAreVisible()) {
                state = TargetState.INITIAL_APPROACH;
            } else if (nav.getRelativeBearing() > 0) {
                drive.rotateRobot(-0.10);
            } else {
                drive.rotateRobot(0.10);
            }
            break;
        case INITIAL_APPROACH:
            if (nav.targetsAreVisible()) {
                if (!nav.cruiseControl(200)) {
                    drive.moveRobot();
                } else {
                    state = TargetState.AT_TARGET;
                    bearing = nav.getRelativeBearing();
                    drive.moveRobot(0.0, 0.0, 0.0);
                }
            } else {
                RobotLog.i("Lost target %f", nav.getRelativeBearing());
                state = TargetState.LOST_TARGET;
            }
            break;
        case AT_TARGET:
            RobotLog.i("Entering state AT_TARGET");
            if ((nav.getRobotBearing() < 3) && (nav.getRobotBearing() > -3)) {
                state = TargetState.ALIGNED;
            } else if (nav.getRobotBearing() > 0) {
                drive.rotateRobot(0.10);
            } else {
                drive.rotateRobot(-0.10);
            }
            break;
        case ALIGNED:
            RobotLog.i("Entering state ALIGNED");
            drive.moveRobot(0.0, 0.0, 0.0);
            break;
        }


    }
}
