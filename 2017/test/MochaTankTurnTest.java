package test;

/*
 * Created by izzielau on 1/7/2017.
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.robot.Robot;

import opmodes.MochaCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.Drivetrain;
import team25core.FourWheelDirectDrivetrain;
import team25core.LightSensorCriteria;
import team25core.RobotEvent;

@Autonomous(name = "TEST Pivot Turn", group = "5218")
public class MochaTankTurnTest extends team25core.Robot{

    private double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    public DeadReckonPath foo;
    public DeadReckonTask fooTask;
    protected LightSensorCriteria whiteLineRightCriteria;
    protected LightSensorCriteria whiteLineLeftCriteria;
    protected LightSensor rightLight;
    protected LightSensor leftLight;
    protected FourWheelDirectDrivetrain drivetrain;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        foo = new DeadReckonPath();
        foo.addSegment(DeadReckonPath.SegmentType.TURN, 90, .3);
        // foo.setMultiplierSide(MochaCalibration.PIVOT_MULTIPLIER, FourWheelPivotTurnDeadReckon.TurningSide.RIGHT);

        whiteLineRightCriteria = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        whiteLineRightCriteria.setThreshold(0.65);
        whiteLineLeftCriteria = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        whiteLineLeftCriteria.setThreshold(0.65);

    }

    @Override
    public void start() {
        rightLight.enableLed(true);
        addTask(new DeadReckonTask(this, foo, drivetrain, whiteLineRightCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.SENSOR_SATISFIED) {

                }
            }
        });
    }
}
