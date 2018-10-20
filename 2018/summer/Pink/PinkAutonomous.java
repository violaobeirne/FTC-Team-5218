package summer.Pink;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import summer.SummerCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TwoWheelDirectDrivetrain;

/**
 * Created by Lizzie on 5/17/2018.
 */

public class PinkAutonomous extends Robot {
    private DcMotor Left;
    private DcMotor Right;

    private Servo servo;

    private TwoWheelDirectDrivetrain drivetrain;
    private DeadReckonPath movePath;
    private DeadReckonTask moveTask;

    @Override
    public void init() {
        Left = hardwareMap.dcMotor.get("Right");
        Right = hardwareMap.dcMotor.get("Left");

        drivetrain = new TwoWheelDirectDrivetrain(Right, Left);
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
