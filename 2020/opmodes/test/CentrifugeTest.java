package opmodes.test;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;

import java.util.HashMap;

import team25core.CentrifugeTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTaskChain;

/**
 * Created by Lizzie on 2/10/2020.
 */
@Autonomous(name = "Centrifuge Test")
@Disabled
public class CentrifugeTest extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private MechanumGearedDrivetrain drivetrain;
    HashMap<MotorPackage.MotorLocation, MotorPackage> motorMap;

    private RobotTaskChain taskChain;

    @Override
    public void init ()
    {
        frontLeft = hardwareMap.get(DcMotorEx.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotorEx.class, "frontRight");
        backLeft = hardwareMap.get(DcMotorEx.class, "backLeft");
        backRight = hardwareMap.get(DcMotorEx.class, "backRight");

        motorMap = new HashMap<>();
        motorMap.put(MotorPackage.MotorLocation.FRONT_LEFT, new MotorPackage(frontLeft));
        motorMap.put(MotorPackage.MotorLocation.FRONT_RIGHT, new MotorPackage(frontRight));
        motorMap.put(MotorPackage.MotorLocation.BACK_LEFT, new MotorPackage(backLeft));
        motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight));
        //motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight, 0.815, MotorPackage.OffsetPolarity.POLARITY_POSITIVE));

        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontLeft);
        taskChain = new RobotTaskChain(this);
    }

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void start()
    {
        initialMove();
        addTask(taskChain);
    }

    public void initialMove()
    {
        taskChain.addTask(new CentrifugeTask(this, drivetrain, 33, 0.8, false));
    }
}
