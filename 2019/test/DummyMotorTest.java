package test;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 1/12/2019.
 */
@Autonomous(name = "Dummy Drive Test")
@Disabled
public class DummyMotorTest extends Robot {
    private DcMotor backLeftDummyMotor;
    private DcMotor backRightDummyMotor;
    private DcMotor frontLeftDummyMotor;
    private DcMotor frontRightDummyMotor;
    private FourWheelDirectDrivetrain drivetrain;
    private DeadReckonPath path;
    private DeadReckonTask task;

    @Override
    public void init() {
        backLeftDummyMotor = hardwareMap.dcMotor.get("backLeft");
        backRightDummyMotor = hardwareMap.dcMotor.get("backRight");
        frontLeftDummyMotor = hardwareMap.dcMotor.get("frontLeft");
        frontRightDummyMotor = hardwareMap.dcMotor.get("frontRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRightDummyMotor, backRightDummyMotor, frontLeftDummyMotor, backLeftDummyMotor);
        drivetrain.encodersOn();
        drivetrain.resetEncoders();
        path = new DeadReckonPath();
        path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, 0.5);
        task = new DeadReckonTask(this, path, drivetrain);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        addTask(task);
    }
}
