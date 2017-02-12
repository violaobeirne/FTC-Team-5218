package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import javax.xml.validation.TypeInfoProvider;

import opmodes.MochaCalibration;
import team25core.AlignWithWhiteLineTask;
import team25core.DeadReckon;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.FourWheelDirectDrivetrain;
import team25core.LightSensorCriteria;
import team25core.MonitorMotorTask;
import team25core.OpticalDistanceSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TwoWheelDirectDrivetrain;

@Autonomous(name = "TEST Align", group = "TEST")
public class MochaAlignWithLineTest extends Robot {

    private DcMotor motorFR;
    private DcMotor motorFL;
    private DcMotor motorBR;
    private DcMotor motorBL;
    private LightSensorCriteria leftSeeWhite;
    private LightSensorCriteria leftSeeBlack;
    private LightSensorCriteria rightSeeWhite;
    private LightSensorCriteria rightSeeBlack;
    private FourWheelDirectDrivetrain drivetrain;
    private LightSensor leftLight;
    private LightSensor rightLight;
    private AlignWithWhiteLineTask alignTask;

    private static final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private static final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private static final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private static final double PIVOT_MOD = MochaCalibration.PIVOT_MULTIPLIER;

    @Override
    public void handleEvent(RobotEvent e)
    {
        RobotLog.i("Received event " + e.toString());

    }

    @Override
    public void init()
    {

        rightLight = hardwareMap.lightSensor.get("lightRight");
        rightLight.enableLed(true);

        leftLight = hardwareMap.lightSensor.get("lightLeft");
        leftLight.enableLed(true);

        motorFR = hardwareMap.dcMotor.get("motorFR");
        motorFL = hardwareMap.dcMotor.get("motorFL");
        motorBR = hardwareMap.dcMotor.get("motorBR");
        motorBL = hardwareMap.dcMotor.get("motorBL");

        leftSeeWhite = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        leftSeeBlack = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);
        rightSeeWhite = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        rightSeeBlack = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);

        drivetrain = new FourWheelDirectDrivetrain(TICKS_PER_INCH, PIVOT_MOD, motorFR, motorBR, motorFL, motorBL);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        alignTask = new AlignWithWhiteLineTask(this, 0, drivetrain, leftSeeBlack, leftSeeWhite, rightSeeBlack, rightSeeWhite);

    }

    @Override
    public void start()
    {
        addTask(alignTask);
    }

}
