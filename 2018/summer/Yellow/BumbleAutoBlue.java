package team25core;
/*
 * FTC Team 25: elizabeth, August 12, 2018
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.Servo;

@Autonomous(name = "BumbleBotsAuto BLUE")
//@Disabled
public class BumbleAutoBlue extends Robot {

    private DcMotor Left;
    private DcMotor Right;
    private DcMotor leftIn;
    private DcMotor rightIn;
    private Servo jewel;

    private Alliance alliance;

    private DeadReckonPath path;
    private DeadReckonTask task;


    private static final int TICKS_PER_INCH = 45;
    private static final int TICKS_PER_DEGREE = 22;
    private final static double STRAIGHT_SPEED= 0.7;
    private final static double TURN_SPEED = 0.7;

    private TwoWheelDirectDrivetrain drivetrain;


    @Override
    public void handleEvent(RobotEvent e)
    {
        // Nothing
    }

    @Override
    public void init()
    {
        // Hardware mapping
        Left = hardwareMap.dcMotor.get("left");
        Right = hardwareMap.dcMotor.get("right");
        leftIn = hardwareMap.dcMotor.get("leftIn");
        rightIn = hardwareMap.dcMotor.get("rightIn");
        jewel = hardwareMap.servo.get("jewel");

        Left.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        Right.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        Left.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        Right.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        path = new DeadReckonPath();
        Left.setDirection(DcMotorSimple.Direction.REVERSE);
        Right.setDirection(DcMotorSimple.Direction.FORWARD);

        path.addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.5);

        int position = Left.getCurrentPosition();
        telemetry.addData("Encoder Position", position);


        drivetrain = new TwoWheelDirectDrivetrain(Left, Right);

    }


    @Override
    public void start()
    {
        jewel.setPosition(170);

        task = new DeadReckonTask(this, path, drivetrain);
        addTask(task);

    }

    public void stop()
    {
        if (task != null) {
            task.stop();
        }
    }

}