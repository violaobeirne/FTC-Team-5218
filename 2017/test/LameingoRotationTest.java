package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.ElapsedTime;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.AlignWithWhiteLineTask;
import team25core.Drivetrain;
import team25core.OpticalDistanceSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TwoWheelDirectDrivetrain;

import static test.LameingoRotationTest.RotationState.LEFT;
import static test.LameingoRotationTest.RotationState.PIVOT_LEFT;
import static test.LameingoRotationTest.RotationState.PIVOT_RIGHT;
import static test.LameingoRotationTest.RotationState.RIGHT;

@Autonomous(name = "TEST Rotation", group = "AutoTest")
public class LameingoRotationTest extends OpMode {

    private DcMotor right;
    private DcMotor left;
    private TwoWheelDirectDrivetrain drivetrain;
    ElapsedTime timer = new ElapsedTime(ElapsedTime.Resolution.SECONDS);

    private final static double SPEED = 0.1;
    private final static double PIVOT_MOD = 3.17;

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
        right = hardwareMap.dcMotor.get("rightMotor");
        left = hardwareMap.dcMotor.get("leftMotor");

        drivetrain = new TwoWheelDirectDrivetrain(0, PIVOT_MOD, right, left);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
    }

    @Override
    public void start()
    {
        state = RIGHT;
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
                drivetrain.turn(SPEED);
            }
            break;
        case LEFT:
            telemetry.addData("Rotation", "Left");
            if (timer.time() >= 3) {
                timer.reset();
                state = PIVOT_RIGHT;
                drivetrain.stop();
            } else {
                drivetrain.turn(-SPEED);
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
