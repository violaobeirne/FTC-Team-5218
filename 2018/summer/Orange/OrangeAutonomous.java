package summer;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 5/17/2018.
 */

public class GreenAutonomous extends Robot {
    private DcMotor frontLeft;
    private DcMotor backLeft;
    private DcMotor frontRight;
    private DcMotor backRight;

    private Servo servo;

    private FourWheelDirectDrivetrain drivetrain;
    private DeadReckonPath movePath;
    private DeadReckonTask moveTask;

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontL");
        backLeft = hardwareMap.dcMotor.get("backL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backRight = hardwareMap.dcMotor.get("backR");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        movePath = new DeadReckonPath();
        moveTask = new DeadReckonTask(this, movePath, drivetrain);

        // copy and paste segments in order to create a dead reckon path
        movePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5.0, SummerCalibration.MOVE_SPEED);
        movePath.addSegment(DeadReckonPath.SegmentType.TURN, 90, SummerCalibration.TURN_SPEED);
    }

    @Override
    public void start() {
        addTask(moveTask);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
