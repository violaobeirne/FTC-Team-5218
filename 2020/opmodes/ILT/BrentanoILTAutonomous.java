package opmodes.ILT;

import com.qualcomm.hardware.lynx.LynxModule;
import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import java.util.HashMap;
import java.util.List;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DistanceSensorCriteria;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTaskChain;
import team25core.SingleShotTimerTask;
import test.SkystoneDetectionTask;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "5218 ILT Skystone Autonomous")
public class BrentanoILTAutonomous extends Robot {

    private final static String TAG = "SternILTAutonomous";

    public enum ArmToggle {
        STOW_ARM,
        DEPLOY_ARM
    }

    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT,
    }

    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo leftStoneArm;
    private Servo rightStoneArm;
    private Servo purpleStoneArm;
    // private Servo leftArm;
    // private Servo rightArm;
    private MechanumGearedDrivetrain drivetrain;
    HashMap<MotorPackage.MotorLocation, MotorPackage> motorMap;

    // sensor
    private ColorSensor leftColorSensor;
    private ColorSensor rightColorSensor;
    private ColorSensor purpleColorSensor;
    private DistanceSensor leftDistanceSensor;
    private DistanceSensor rightDistanceSensor;
    private DistanceSensor purpleDistanceSensor;

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item stone;

    // autonomous variable declaration
    private AllianceColor allianceColor;
    private int MULTIPLIER = MiyazakiCalibration.ALLIANCE_MULTIPLIER;
    private int stoneOffset = 0;

    private RobotTaskChain taskChain;

    @Override
    public void init ()
    {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.get(DcMotorEx.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotorEx.class, "frontRight");
        backLeft = hardwareMap.get(DcMotorEx.class, "backLeft");
        backRight = hardwareMap.get(DcMotorEx.class, "backRight");

        leftStoneArm = hardwareMap.get(Servo.class, "leftStoneArm");
        rightStoneArm = hardwareMap.get(Servo.class, "rightStoneArm");
        // leftArm = hardwareMap.get(Servo.class, "leftArm");
        // rightArm = hardwareMap.get(Servo.class, "rightArm");

        List<LynxModule> allHubs = hardwareMap.getAll(LynxModule.class);
        for (LynxModule module : allHubs) {
            module.setBulkCachingMode(LynxModule.BulkCachingMode.AUTO);
        }

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

        // gamepad and telemetry initialization
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);
        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        stone = telemetry.addData("Stone Position: ", "NOT SELECTED");

        // sensor initialization
        leftColorSensor = hardwareMap.get(RevColorSensorV3.class, "leftColorSensor");
        rightColorSensor = hardwareMap.get(RevColorSensorV3.class, "rightColorSensor");
        leftDistanceSensor = hardwareMap.get(DistanceSensor.class, "leftColorSensor");
        rightDistanceSensor = hardwareMap.get(DistanceSensor.class, "rightColorSensor");

        // servo initialization
        // leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        // rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);

        // sky stone path initialization
        allianceColor = allianceColor.DEFAULT;

        taskChain = new RobotTaskChain(this);
    }

    @Override
    public void handleEvent (RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamePadSelection(event);
        }
    }

    public void handleGamePadSelection (GamepadTask.GamepadEvent event)
    {
        switch (event.kind){
            case BUTTON_X_DOWN:
                allianceColor = allianceColor.BLUE;
                MULTIPLIER = 1;
                purpleColorSensor = rightColorSensor;
                purpleDistanceSensor = rightDistanceSensor;
                purpleStoneArm = rightStoneArm;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = allianceColor.RED;
                MULTIPLIER = -1;
                purpleColorSensor = leftColorSensor;
                purpleDistanceSensor = leftDistanceSensor;
                purpleStoneArm = leftStoneArm;
                alliance.setValue("RED");
                break;
        }
    }

    @Override
    public void start ()
    {
        /*
         * Sets up a robot task chain.  All operations here will run serialized.
         */
        initialMove();
        findStoneEdge();
        detectSkyStone();
        approachStone();
        doWait(200);
        pullStoneBack();
        deliverFirstStone();
        moveToSecondStone();
        /*
         * Start the task chain.
         */
        addTask(taskChain);
    }

    public void doWait(int ms)
    {
        RobotLog.ii(TAG, "Setup doWait");
        taskChain.addTask(new SingleShotTimerTask(this, ms));
    }

    public void initialMove () // 1
    {
        // taskChain.addTask(new CentrifugeTask(this, drivetrain, 33, 0.8, false));
        RobotLog.ii(TAG, "Setup initialMove");
        DeadReckonPath initialPath;
        initialPath = new DeadReckonPath();
        initialPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 27.5, 0.4);
        taskChain.addTask(new DeadReckonTask(this, initialPath, drivetrain));
    }

    public void findStoneEdge() // 2
    {
        RobotLog.ii(TAG, "Setup findStoneEdge");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 5);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, MULTIPLIER * -.1);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            @Override
            public void start() {
                super.start();
                RobotLog.ii(TAG, "Starting findStoneEdge");
            }
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.ii(TAG, "Found stone edge");
                        this.disableSensors();
                        robot.removeTask(this);
                        drivetrain.resetEncoders();
                        drivetrain.encodersOn();
                        drivetrain.straight(-0.1 * MULTIPLIER);
                        break;
                }
            }
        });
    }

    public void detectSkyStone() // 3
    {
        RobotLog.ii(TAG, "Setup detectSkystone");
        taskChain.addTask(new SkystoneDetectionTask(this, purpleColorSensor, purpleDistanceSensor) {
            @Override
            public void start() {
                super.start();
                RobotLog.ii(TAG, "Starting detectSkyStone");
            }

            public void handleEvent(RobotEvent e) {
                SkystoneDetectionEvent event = (SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        RobotLog.ii(TAG, "Color detected");
                        stone.setValue("DETECTED");
                        stoneOffset = drivetrain.getCurrentPosition() / MiyazakiCalibration.ENCODERS_PER_INCH;
                        drivetrain.stop();
                        break;
                }
            }
        });
    }

    public void approachStone() // 4
    {
        RobotLog.ii(TAG, "Setup approachStone");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 2);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 100, .2);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            @Override
            public void start() {
                super.start();
                RobotLog.ii(TAG, "Starting approachStone");
            }
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.ii(TAG, "Finished approaching stone");
                        drivetrain.stop();
                        robot.removeTask(this);
                        moveStoneArms(ArmToggle.DEPLOY_ARM);
                        break;
                }
            }
        });
    }

    public void pullStoneBack() // 5
    {
        RobotLog.ii(TAG, "Setup pullStoneBack");
        DeadReckonPath pullBack = new DeadReckonPath();
        pullBack.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, -0.4);
        taskChain.addTask(new DeadReckonTask(this, pullBack, drivetrain) {
            @Override
            public void start() {
                super.start();
                RobotLog.ii(TAG, "Starting pullStoneBack");
            }

        });
    }
    public void deliverFirstStone () // 6
    {
        int park = 0;
        RobotLog.ii(TAG, "Setup skystoneOffset");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset + 45, MULTIPLIER * 0.8);
        taskChain.addTask(new DeadReckonTask(this, offsetStonePath, drivetrain) {
            @Override
            public void start() {
                super.start();
                RobotLog.ii(TAG, "Starting deliverFirstStone");
            }
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.ii(TAG, "StoneOffset: ", stoneOffset);
                        moveStoneArms(ArmToggle.STOW_ARM);
                        RobotLog.ii(TAG, "Finshed delivering first stone.");
                        break;
                }
            }
        });
    }

    public void moveToSecondStone() // 7
    {
        RobotLog.ii(TAG, "Setup moveToSecondStone");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        int park = 10;
        if(stoneOffset < 10) {
            park = 15;
        }
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset + park, -MULTIPLIER * 0.8);
        taskChain.addTask(new DeadReckonTask(this, offsetStonePath, drivetrain));
    }

    public void moveStoneArms(ArmToggle arm)
    {
        RobotLog.ii(TAG, "moveStoneArms");
        switch(arm) {
            case DEPLOY_ARM:
                switch (allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                        RobotLog.ii(TAG, "Deployed right stone arm.");
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                        RobotLog.ii(TAG, "Deployed left stone arm.");
                        break;
                }
                break;

            case STOW_ARM:
                switch (allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
                        RobotLog.ii(TAG, "Stowed right stone arm.");
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                        RobotLog.ii(TAG, "Stowed left stone arm.");
                        break;
                }
                break;
        }
    }
}
