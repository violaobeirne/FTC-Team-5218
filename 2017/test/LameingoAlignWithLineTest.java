package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.AlignWithWhiteLineTask;
import team25core.LightSensorCriteria;
import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TwoWheelDirectDrivetrain;

@Autonomous(name = "TEST AlignWithLine", group = "AutoTest")
@Disabled
public class LameingoAlignWithLineTest extends Robot {

    private DcMotor right;
    private DcMotor left;
    private LightSensorCriteria leftSeeWhite;
    private LightSensorCriteria leftSeeBlack;
    private LightSensorCriteria rightSeeWhite;
    private LightSensorCriteria rightSeeBlack;
    private TwoWheelDirectDrivetrain drivetrain;
    private LightSensor leftLight;
    private LightSensor rightLight;
    private AlignWithWhiteLineTask alignTask;

    private static final double LIGHT_MIN = 1.35;
    private static final double LIGHT_MAX = 2.01;

    @Override
    public void handleEvent(RobotEvent e)
    {
        RobotLog.i("Received event " + e.toString());

    }

    @Override
    public void init()
    {
        leftLight = hardwareMap.lightSensor.get("left");
        rightLight = hardwareMap.lightSensor.get("right");
        right = hardwareMap.dcMotor.get("right");
        left = hardwareMap.dcMotor.get("left");

        leftSeeWhite = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        leftSeeBlack = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);
        rightSeeWhite = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        rightSeeBlack = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);

        drivetrain = new TwoWheelDirectDrivetrain(0, right, left);

        alignTask = new AlignWithWhiteLineTask(this, drivetrain, leftSeeBlack, leftSeeWhite, rightSeeBlack, rightSeeWhite);
    }

    @Override
    public void start()
    {
        addTask(alignTask);
    }

}
