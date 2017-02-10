package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.ElapsedTime;

import team25core.Drivetrain;
import team25core.FourWheelDirectDrivetrain;
import team25core.TwoWheelDirectDrivetrain;

import static test.MochaRotationTest.RotationState.LEFT;
import static test.MochaRotationTest.RotationState.PIVOT_LEFT;
import static test.MochaRotationTest.RotationState.PIVOT_RIGHT;
import static test.MochaRotationTest.RotationState.RIGHT;

@Autonomous(name = "TEST Mocha Rotation", group = "AutoTest")
public class MochaRotationTest extends OpMode {

    private FourWheelDirectDrivetrain drivetrain;
    ElapsedTime timer = new ElapsedTime(ElapsedTime.Resolution.SECONDS);

    private final static double SPEED = 0.1;
    private final static double PIVOT_MOD = 2.0;
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private LightSensor rightLight;
    private LightSensor leftLight;

    enum RotationState {
        RIGHT,
        LEFT,
        PIVOT_RIGHT,
        PIVOT_LEFT,
    };

    RotationState state;

    @Override
    public void init()
    {
        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        drivetrain = new FourWheelDirectDrivetrain(0, PIVOT_MOD, backLeft, frontLeft, backRight, frontRight);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
    }

    @Override
    public void start()
    {
        state = PIVOT_LEFT;
        timer.reset();
    }

    @Override
    public void loop()
    {
        switch (state) {
        case RIGHT:
            telemetry.addData("Rotation", "Right");
            if (timer.time() >= 3) {
                timer.reset();
                state = LEFT;
                drivetrain.stop();
            } else {
                drivetrain.turnRight(SPEED);
            }
            break;
        case LEFT:
            telemetry.addData("Rotation", "Left");
            if (timer.time() >= 3) {
                timer.reset();
                state = PIVOT_RIGHT;
                drivetrain.stop();
            } else {
                drivetrain.turnLeft(SPEED);
            }
            break;
        case PIVOT_RIGHT:
            telemetry.addData("Rotation", "Pivot Right over right");
            if (timer.time() >= 10) {
                timer.reset();
                state = PIVOT_LEFT;
                drivetrain.stop();
            } else {
                drivetrain.pivotTurn(Drivetrain.PivotSide.RIGHT_OVER_RIGHT, SPEED);
            }
            break;
        case PIVOT_LEFT:
            telemetry.addData("Rotation", "Pivot Left over left");
            if (timer.time() >= 10) {
                timer.reset();
                state = RIGHT;
                drivetrain.stop();
            } else {
                drivetrain.pivotTurn(Drivetrain.PivotSide.LEFT_OVER_LEFT, SPEED);
            }
            break;
        }
    }

}
