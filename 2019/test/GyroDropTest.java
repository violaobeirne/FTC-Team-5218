package test;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMUGyroTask;
import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;

/**
 * Created by Lizzie on 11/24/2018.
 */
@Autonomous(name = "Gyro Drop Test")
public class GyroDropTest extends Robot {
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;
    private DeadReckonPath movePath;
    private DeadReckonTask moveTask;
    BNO055IMU imu;
    private Telemetry.Item gyroItem;
    private IMUGyroTask gyroTask;
    private double turnSpeed = VivaldiCalibration.TURN_SPEED;



    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        movePath = new DeadReckonPath();
        movePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);
        moveTask = new DeadReckonTask(this, movePath, drivetrain);

        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        imu = hardwareMap.get(BNO055IMU.class, "IMU");
        gyroItem = telemetry.addData("Gyro state:", "Not at target");
        handleGyroEvent();
    }

    public void handleGyroEvent ()
    {
        gyroTask = new IMUGyroTask(this, imu, 0, true) {
          @Override
          public void handleEvent (RobotEvent event) {
              if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                  drivetrain.stop();
                  addTask(moveTask);
              } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                  drivetrain.turn(turnSpeed / 2);
              }
          }
        };
        gyroTask.init();
    }

    @Override
    public void start() {
        liftUp();
    }

    public void liftUp()
    {
        RobotLog.i("251 - Going UP");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE L");
                    liftLeft.setPower(0.0);
                    leftLanded = true;
                    if (rightLanded = true) {
                        initialStraighten();
                    }
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE R");
                    liftRight.setPower(0.0);
                    rightLanded = true;
                    if (leftLanded == true) {
                        initialStraighten();
                    }
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void initialStraighten()
    {
        addTask(gyroTask);
        drivetrain.turn(-turnSpeed);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
