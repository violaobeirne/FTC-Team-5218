package opmodes.LM3;

import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import java.util.HashMap;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DistanceSensorCriteria;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;
import test.SkystoneDetectionTask;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "SKETCH PARKING AUTO")
public class SketchParkAuto extends Robot {

    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private MechanumGearedDrivetrain drivetrain;
    HashMap<MotorPackage.MotorLocation, MotorPackage> motorMap;

    @Override
    public void init ()
    {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");

        motorMap = new HashMap<>();
        motorMap.put(MotorPackage.MotorLocation.FRONT_LEFT, new MotorPackage(frontLeft));
        motorMap.put(MotorPackage.MotorLocation.FRONT_RIGHT, new MotorPackage(frontRight));
        motorMap.put(MotorPackage.MotorLocation.BACK_LEFT, new MotorPackage(backLeft));
        motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight, 0.815, MotorPackage.OffsetPolarity.POLARITY_POSITIVE));

        drivetrain = new MechanumGearedDrivetrain(60, motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontLeft);
    }

    @Override
    public void handleEvent (RobotEvent e)
    {
    }


    @Override
    public void start ()
    {
        pause();
    }

    public void pause ()
    {
        addTask(new SingleShotTimerTask(this, 10000) {
            public void handleEvent(RobotEvent e) {
                SingleShotTimerEvent event = (SingleShotTimerEvent) e;
                switch(event.kind) {
                    case EXPIRED:
                        initialMove();
                        break;
                }
            }
        });
    }

    public void initialMove ()
    {
        RobotLog.i("163 initialMove");
        DeadReckonPath initialPath = new DeadReckonPath();
        initialPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 28, 0.5);
        addTask(new DeadReckonTask(this, initialPath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        break;
                }
            }
        });
    }
}
