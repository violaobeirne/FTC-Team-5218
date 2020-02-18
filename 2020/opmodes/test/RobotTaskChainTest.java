package opmodes.test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import java.util.HashMap;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTaskChain;
import team25core.StandardFourMotorRobot;

@Autonomous(name="Robot Task Chain Test")
public class RobotTaskChainTest extends StandardFourMotorRobot {

    private MechanumGearedDrivetrain drivetrain;

    RobotTaskChain chain = new RobotTaskChain(this);

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    public void doObjectiveOne()
    {
        DeadReckonPath path = new DeadReckonPath();
        path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.3);
        chain.addTask(new DeadReckonTask(this, path, drivetrain));
    }

    public void doObjectiveTwo()
    {
        DeadReckonPath path = new DeadReckonPath();
        path.addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.3);
        chain.addTask(new DeadReckonTask(this, path, drivetrain));
    }

    public void doObjectiveThree()
    {
        DeadReckonPath path = new DeadReckonPath();
        path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.3);
        chain.addTask(new DeadReckonTask(this, path, drivetrain));
    }

    @Override
    public void init()
    {
        super.init();
        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontLeft);

        doObjectiveOne();
        doObjectiveTwo();
        doObjectiveThree();
    }

    @Override
    public void start()
    {
        this.addTask(chain);
    }
}
